import csv
import random, os, json, sqlite3
from datetime import datetime
from zoneinfo import ZoneInfo
from flask import Flask, render_template_string, request, abort, jsonify
import secrets

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
CSV_DIR = os.path.join(BASE_DIR, "stimuli")
_seen_keys: set[tuple[str, int]] = set()

# Each participant plays one game type, then the other (12 rounds each), in either Take or Give mode.
N_PODS = 2
WITHIN_SUBJECT = "game_type"
GAME_TYPES = ["adversarial", "cooperative"]
DRAW_MODES = [0, 1]          # draw_is_take: 0 = Give, 1 = Take
STAGES_ROUNDS = [12]
VARIABLE_PAYOFF = 0.10       # $ per point
BASE_PAYOFF_ADV = 1.00
BASE_PAYOFF_COOP = 0
BALANCED_CATS = 1            # rounds are balanced across stimulus categories


def _read_csv_rows(fname: str) -> list[dict]:
    with open(fname, newline="") as fh:
        rows = list(csv.DictReader(fh))
    for idx, row in enumerate(rows):
        row["__idx"] = idx
    return rows

def _draw_balanced_block(
    rows: list[dict],
    category_var: str,
    n_draw: int,
    seen: set[tuple[str,int]],
    fname: str
) -> list[dict]:
    """Draw n_draw rows not yet in `seen`, cycling through one row per category in shuffled blocks."""
    groups: dict[str,list[dict]] = {}
    for row in rows:
        cat = row.get(category_var)
        if cat is None:
            abort(400, f"Missing '{category_var}' in {fname}")
        groups.setdefault(cat, []).append(row)

    k = len(groups)
    if n_draw % k != 0:
        raise ValueError(f"{n_draw} not a multiple of {k} categories in {fname}.")

    for lst in groups.values():
        random.shuffle(lst)

    out: list[dict] = []
    used_ptr: dict[str,int] = {cat: 0 for cat in groups}

    while len(out) < n_draw:
        block: list[dict] = []
        for cat in sorted(groups):
            while used_ptr[cat] < len(groups[cat]):
                candidate = groups[cat][used_ptr[cat]]
                used_ptr[cat] += 1
                key = (fname, candidate["__idx"])
                if key in seen:
                    continue
                seen.add(key)
                block.append(candidate)
                break
            else:
                abort(500, f"Ran out of unused rows in category '{cat}' of {fname}.")

        random.shuffle(block)
        out.extend(block)

    return out[:n_draw]


DOT_COLS = ["A1", "A2", "B1", "B2"]
CATEGORY_COLS = ["category_adv_take", "category_adv_give", "category_coop_take", "category_coop_give"]
CSV_COLUMNS = DOT_COLS + CATEGORY_COLS

def stimulus_file(game_type: str, draw_is_take: int) -> tuple[str, str]:
    mode = "take" if draw_is_take else "give"
    game = "adv" if game_type == "adversarial" else "coop"
    fname = os.path.join(CSV_DIR, f"draft_sample{N_PODS}_{mode}_{game}.csv")
    if not os.path.exists(fname):
        abort(500, f"Missing CSV on server: {fname}")
    return fname, f"category_{game}_{mode}"


def build_round_samples(stages_rounds, stages_blobs, stages_gtypes, draw_is_take):
    samples, config = [], []
    _seen_keys.clear()
    for i, n_rounds in enumerate(stages_rounds):
        fname, category_col = stimulus_file(stages_gtypes[i], draw_is_take)
        block = _draw_balanced_block(_read_csv_rows(fname), category_col, n_rounds, _seen_keys, fname)
        for row in block:
            samples.append(row)
            config.append({"stageIndex": i, "blobCount": stages_blobs[i]})
    return samples, config


def get_num_categories(game_type, draw_is_take):
    fname, category_col = stimulus_file(game_type, draw_is_take)
    with open(fname, newline="") as fh:
        return len({row[category_col] for row in csv.DictReader(fh) if row.get(category_col)})


# ---- Database ----

def get_db_connection():
    # ./data next to this file, unless DATA_DIR is set (e.g. a persistent disk on a host)
    data_dir = os.environ.get("DATA_DIR", os.path.join(BASE_DIR, "data"))
    os.makedirs(data_dir, exist_ok=True)
    db_path = os.path.join(data_dir, "game_data.sqlite")
    return sqlite3.connect(db_path)

def create_subjects_table(conn):
    conn.execute("""
      CREATE TABLE IF NOT EXISTS subjects (
        subject_id            INTEGER PRIMARY KEY AUTOINCREMENT,
        qualtrics_id          TEXT,
        condition             TEXT,
        game_type             TEXT,
        draw_is_take          INTEGER,
        ts_welcome            TEXT,
        ts_instructions       TEXT,
        ts_demo               TEXT,
        ts_demo2              TEXT,
        ts_check              TEXT,
        ts_check2             TEXT,
        quiz_attempts         INTEGER,
        check_first_try1      INTEGER,
        check_first_try2      INTEGER,
        check_first_try3      INTEGER,
        check_first_try4      INTEGER,
        check_first_try_pass  INTEGER,
        quiz2_attempts         INTEGER,
        check2_first_try1      INTEGER,
        check2_first_try2      INTEGER,
        check2_first_try3      INTEGER,
        check2_first_try4      INTEGER,
        check2_first_try_pass  INTEGER,
        ts_game               TEXT,
        ts_end                TEXT
      )
    """)
    conn.commit()

def create_actions_table(conn):
    logging_cols = """
      subject_id       INTEGER,
      action_game_type        TEXT,
      action_condition        TEXT,
      round_number     INTEGER,
      round_grp_number INTEGER,
      n_blobs          INTEGER,
      draw_is_take    INTEGER,
      round_start_time TEXT,
      move1            TEXT, timestamp1 TEXT,
      move2            TEXT, timestamp2 TEXT,
      move3            TEXT, timestamp3 TEXT,
      move4            TEXT, timestamp4 TEXT,
      move5            TEXT, timestamp5 TEXT,
      move6            TEXT, timestamp6 TEXT,
      cum_score_you    INTEGER,
      cum_score_opp    INTEGER,
      cum_score_obj    INTEGER,
      round_payoff     REAL,
      round_implemented INTEGER,
      draw_mode        TEXT,
      balanced_cats    INTEGER
    """
    csv_def = ", ".join(f"{col} TEXT" for col in CSV_COLUMNS)

    conn.execute(f"""
      CREATE TABLE IF NOT EXISTS game_actions (
        {logging_cols}
        {', ' + csv_def if csv_def else ''}
      )
    """)
    conn.commit()


# ---- App ----

app = Flask(__name__, static_folder="static", static_url_path="/static")

with app.app_context():
    conn = get_db_connection()
    create_subjects_table(conn)
    create_actions_table(conn)
    conn.close()

def _circle(color: str) -> str:
    return f'<div style="width:14px;height:14px;background:{color};border-radius:50%;margin:2px;"></div>'

def _pod(dots: int) -> str:
    reds  = "".join(_circle("red")  for _ in range(dots))
    blues = "".join(_circle("blue") for _ in range(dots))
    return (
        '<div style="display:inline-block;margin:20px;width:100px;height:100px;'
        'border:2px solid #000;border-radius:50%;position:relative;">'
        f'<div style="position:absolute;top:10%;left:5%;width:40%;display:grid;'
        f'grid-template-columns:repeat(2,auto);grid-row-gap:2px;'
        f'justify-content:center;align-content:start;">{reds}</div>'
        f'<div style="position:absolute;top:10%;right:5%;width:40%;display:grid;'
        f'grid-template-columns:repeat(2,auto);grid-row-gap:2px;'
        f'justify-content:center;align-content:start;">{blues}</div>'
        '</div>'
    )

def pods_block_html() -> str:
    # Two example pods, stacked vertically
    return '<div style="display:flex;flex-direction:column;align-items:center;">' + _pod(2) + _pod(3) + "</div>"


def pick_balanced_group():
    """Assign to the (first game type, Take/Give) cell with the fewest participants so far."""
    conn = get_db_connection()
    counts = {(N_PODS, g, d): 0 for g in GAME_TYPES for d in DRAW_MODES}
    cutoff_date = "2026-02-11"  # start of the main study

    for cond, gt, draw, n in conn.execute(
        """
        SELECT condition, game_type, draw_is_take, COUNT(*) AS n
        FROM subjects
        WHERE ts_welcome IS NOT NULL
        AND substr(ts_welcome, 1, 10) >= ?
        GROUP BY condition, game_type, draw_is_take
        """,
        (cutoff_date,)
    ):
        try:
            key = (int(cond), gt, int(draw))
        except (TypeError, ValueError):
            continue
        if key in counts:
            counts[key] = n
    conn.close()

    min_n = min(counts.values())
    choices = [k for k, v in counts.items() if v == min_n]
    return secrets.choice(choices)

@app.route("/")
def index():
    condition_pods, game_type, draw_is_take = pick_balanced_group()
    qualtrics_id = request.args.get("qualtrics_id", default="", type=str)

    other_gt = [g for g in GAME_TYPES if g != game_type][0]
    stages_gtypes = [game_type, other_gt]
    stages_rounds = [STAGES_ROUNDS[0]] * 2
    stages_blobs = [condition_pods] * 2
    round_samples, round_config = build_round_samples(stages_rounds, stages_blobs, stages_gtypes, draw_is_take)
    num_categories = get_num_categories(game_type, draw_is_take)

    # Instruction text for the first game
    partner    = "Opponent" if game_type == "adversarial" else "Teammate"
    metric     = (
        "Score Difference = Your Score – Opponent Score."
        if game_type == "adversarial"
        else "Score Sum = Your Score + Teammate Score."
    )
    base_payoff = BASE_PAYOFF_ADV if game_type == "adversarial" else BASE_PAYOFF_COOP
    payoff_text = (
        f"Your bonus = (${VARIABLE_PAYOFF:.2f} × Score Difference) + ${base_payoff:.2f}."
        if game_type == "adversarial"
        else f"Your bonus = ${VARIABLE_PAYOFF:.2f} × Score Sum."
    )
    if draw_is_take:
        rules_text = (
            f"In each round, you will choose the first pod and take it for yourself.<br>"
            f"Then your {partner.lower()} will get the remaining pod."
        )
    else:
        rules_text = (
            f"In each round, you will choose the first pod and give it to your {partner.lower()}.<br>"
            f"Then you will get the remaining pod."
        )

    give_take_html = (
        f"Every time you get a pod, you get points equal to its "
        f"<span style='color:red;font-weight:bold;'>red</span> dots, and your {partner.lower()} gets nothing.<br>"
        f"Every time your {partner.lower()} gets a pod, they get points equal to its "
        f"<span style='color:blue;font-weight:bold;'>blue</span> dots, and you get nothing.<br>"
    ) if draw_is_take else (
        f"Every time your {partner.lower()} gets a pod, they get points equal to its "
        f"<span style='color:blue;font-weight:bold;'>blue</span> dots, and you get nothing.<br>"
        f"Every time you get a pod, you get points equal to its "
        f"<span style='color:red;font-weight:bold;'>red</span> dots, and your {partner.lower()} gets nothing.<br>"
    )

    bottom_html = (
        give_take_html +
        f"At the end of each round, new pods will appear.<br><br>"
        f"<span style='font-weight:bold;'>{metric}<br>{payoff_text}</span>"
    )
    pods_html = pods_block_html()

    instructions_html = f"""
    You are going to play {STAGES_ROUNDS[0]} rounds of a game with a computer <b><u>{partner.upper()}.</u></b><br>
    {rules_text}<br>
    The pods will look like this, with varying numbers of dots:<br>
    {pods_html}
    <div id="instrBottomText">{bottom_html}</div>
    """

    page_template = """
    <!DOCTYPE html>
    <html>
    <head>
      <meta charset='utf-8'>
      <title>experiment</title>
      <link rel="stylesheet" href="/static/style.css">
    </head>
    <body>
      <div id="gameRoot">

    <!-- ─── 0 Welcome ───────────────────────────────────────────── -->
    <div id='screenWelcome' class='screen active'>
        <div class='center'>
        <h2 style="font-size:2em;max-width:850px;margin:0 auto;"><br>
            On the next screens, you will learn how to play a strategic game.<br><br>
            You will then play <strong>__NROUNDS__ rounds</strong> of the game.<br><br>
            Then, you will learn about a change in the game rules.<br><br>
            You will then play <strong>__NROUNDS__ rounds</strong> of the game (with the changed rules).<br><br>
            Each round of the game only involves one decision for you, so the rounds will move quickly.<br><br>
            Pay close attention to the instructions and demonstration, as your <em>bonus payment</em> will depend on your performance in one randomly chosen round.
        </h2>
        <button id='btnBeginWelcome' style="margin-top:1.5em;font-size:1.2em;">
            Click to Begin
        </button>
        </div>
    </div>

    <!-- ─── 1 Instructions ──────────────────────────────────────── -->
    <div id='screenInstructions' class='screen'>
      <h1 class='center'>Instructions</h1>
      <div id='instructionsText'>__INSTR_HTML__</div>
      <div class='center' style='margin:1.5em 0;'>
        <button id='btnInstrContinue'>Continue</button>
      </div>
    </div>

    <!-- 2 Demo -->
    <div id='screenDemo' class='screen'>
      <h1 class='center'>Demo</h1>
      <div id="demoLeftBox"  class="sideBox">
          <div class="sideLabel">You</div>
          <div id="demoLeftGrid"  class="boxGrid"></div>   <!-- NEW -->
      </div>
      <div id="demoRightBox" class="sideBox">
          <div class="sideLabel" id="demoRightLabel"></div>
          <div id="demoRightGrid" class="boxGrid"></div>   <!-- NEW -->
      </div>
      <div class="center contentBlock">
        Watch the following demonstration to see how the game works.<br><br>
        <button id="btnDemoPlay">Play Demo</button>
      </div>

      <!-- exact same pods as Instructions -->
      <div id="demoStaticPods" class="center">__PODS_HTML__</div>
      <div id="demoNarration"></div>

      <!-- bottom rules text -->
      <div id="demoBottomText" class="center contentBlock">__BOTTOM_HTML__</div>

      <div class='center'><button id='btnDemoContinue'>Continue</button></div>
    </div>

    <!-- 3 Check Your Understanding -->
    <div id='screenQuiz' class='screen'>
      <h1 class='center'>Check Your Understanding</h1>

      <div id="quizLeftBox"  class="sideBox">
          <div class="sideLabel">You</div>
          <div id="quizLeftGrid"  class="boxGrid"></div>
      </div>
      <div id="quizRightBox" class="sideBox">
          <div class="sideLabel" id="quizRightLabel"></div>
          <div id="quizRightGrid" class="boxGrid"></div>
      </div>
      <div class="center contentBlock">
        Now click to see a different possible sequence of draws.<br><br>
        <button id="btnQuizPlay">Play Demo</button>
      </div>

      <!-- SAME pods block so the layout matches Demo -->
      <div id="quizStaticPods" class="center">__PODS_HTML__</div>
      <div id="quizNarration"></div>

      <div id="quizFormContainer" style="margin-top:20px;"></div>

      <!-- bottom rules text -->
      <div id="quizBottomText" class="center contentBlock">__BOTTOM_HTML__</div>

    </div>

    <!-- ─── 4 Game ──────────────────────────────────────────────── -->
    <div id='screenGame' class='screen'>
      <div id='top-bar'>
        Round <span id='roundNumber'></span> of <span id='totalRounds'></span>
      </div>

      <!-- modal -->
      <div id='modal-overlay'>
        <div id='modal-content'>
          <div id='modal-message'></div>
          <button id='modal-button' onclick='closeModal()'>OK</button>
        </div>
      </div>

        <!-- helper text -->
        <div id='instructions'>
          <strong>Rules:</strong> __RULES_TEXT__<br>
          Every time you draw a pod, you get points equal to its 
          <span style='color:red;font-weight:bold;'>red</span> dots, and your __ROLE__ gets nothing.<br>
          Every time your __ROLE__ draws a pod, they get points equal to its 
          <span style='color:blue;font-weight:bold;'>blue</span> dots, and you get nothing.<br>
          At the end of each round, new pods will appear.<br><br>
          <span style='font-weight:bold;'>__METRIC__<br>__PAYOFF_TEXT__</span>
          

        </div>
      <!-- main grid -->
      <div id='container'>
        <div id='left-area'>
          <div class='score-title'>Your Score</div>
          <div id='playerScore' class='score-value'>0</div>
          <div id='selected-left'></div>
        </div>

        <div id='middle-area'>
          <div id='turnIndicator'></div>
          <div id='middle-grid'></div>
          <div id='scoreDifference'></div>
        </div>

        <div id='right-area'>
          <div class='score-title'>__ROLEU__'s Score</div>
          <div id='opponentScore' class='score-value'>0</div>
          <div id='selected-right'></div>
        </div>
      </div>
    </div>

    <!-- config JSON + scripts -->
    <script id='config' type='application/json'>__CFG__</script>
    <script src='/static/game.js' defer></script>
      </div>
    </body>
<script>
(() => {
  const W = 1280;                                   // design width
  const root = document.getElementById('gameRoot') ||
               document.getElementById('screenGame');

  /* resize logic */
  function fit(){
    const scale = document.documentElement.clientWidth / W;
    root.style.transform       = `scale(${scale})`;
    root.style.transformOrigin = 'top left';

    /* tell Qualtrics the NEW pixel height each time */
    if (window.parent && window.parent !== window){
      window.parent.postMessage(
        { type:'gameResize', height: root.scrollHeight * scale }, '*');
    }
  }

  /* recalc on window resize … */
  window.addEventListener('resize', fit);
  window.addEventListener('DOMContentLoaded', fit);

  /* …and whenever the game DOM changes (e.g. new pods appear) */
  new MutationObserver(fit).observe(root, { childList:true, subtree:true });
})();
</script>
    </html>
    """

    cfg_payload = json.dumps({
      "STAGES_ROUNDS":     stages_rounds,
      "STAGES_BLOBS":      stages_blobs,
      "STAGES_GAME_TYPES": stages_gtypes,
      "WITHIN_SUBJECT":    WITHIN_SUBJECT,
      "ROUND_SAMPLES":     round_samples,
      "ROUND_CONFIG":      round_config,
      "CSV_COLUMNS":       CSV_COLUMNS,
      "NUM_CATEGORIES":    num_categories,
      "BASE_PAYOFF_ADV":   BASE_PAYOFF_ADV,
      "BASE_PAYOFF_COOP":  BASE_PAYOFF_COOP,
      "VARIABLE_PAYOFF":   VARIABLE_PAYOFF,
      "GAME_TYPE":         game_type,
      "DRAW_IS_TAKE":     int(draw_is_take),
      "BALANCED_CATS": BALANCED_CATS,
      "ASSIGNMENT": {
            "condition": int(condition_pods),
            "game_type": game_type,
            "draw_is_take": int(draw_is_take),
        },
      "QUALTRICS_ID": qualtrics_id,
    })

    def gsub(txt: str, token: str, val: str) -> str:
        return val.join(txt.split(token))

    page = page_template
    page = gsub(page, "__RULES_TEXT__",  rules_text)
    page = gsub(page, "__METRIC__",      metric)
    page = gsub(page, "__PAYOFF_TEXT__", payoff_text)
    page = gsub(page, "__INSTR_HTML__",  instructions_html)
    page = gsub(page, "__PODS_HTML__",   pods_html)
    page = gsub(page, "__BOTTOM_HTML__", bottom_html)
    page = gsub(page, "__CFG__",         cfg_payload)
    page = gsub(page, "__ROLE__",        partner.lower())
    page = gsub(page, "__ROLEU__",       partner)
    page = gsub(page, "__NROUNDS__",     str(STAGES_ROUNDS[0]))

    return render_template_string(page)

@app.route("/stage_update", methods=["POST"])
def stage_update():
    """Record second-demo or second-quiz timestamps & quiz stats."""
    data  = request.get_json()
    sid   = data.get("subject_id")
    stage = data.get("stage")
    ts    = datetime.now(ZoneInfo("America/New_York")).isoformat()

    conn = get_db_connection()
    if stage == "demo2":
        conn.execute("UPDATE subjects SET ts_demo2=? WHERE subject_id=?", (ts, sid))
    elif stage == "check2":
        # pull quiz stats
        attempts = data.get("attempts")
        f1, f2, f3, f4 = data.get("first_try_results", [None]*4)
        pass_flag = data.get("all_first_try")
        conn.execute("""
          UPDATE subjects SET
            ts_check2=?,
            quiz2_attempts=?,
            check2_first_try1=?, check2_first_try2=?, check2_first_try3=?, check2_first_try4=?,
            check2_first_try_pass=?
          WHERE subject_id=?
        """, (ts, attempts, f1, f2, f3, f4, pass_flag, sid))
    else:
        conn.close()
        return jsonify(error="bad stage"), 400

    conn.commit()
    conn.close()
    return jsonify(status="ok")

@app.route("/subject_new", methods=["POST"])
def subject_new():
    rec = request.get_json(force=True) or {}

    qualtrics_id = rec.get("qualtrics_id", "") or request.args.get("qualtrics_id", "")
    condition    = rec.get("condition", "")
    game_type    = rec.get("game_type", "")
    draw_is_take = int(rec.get("draw_is_take", 1))

    ts = datetime.now(ZoneInfo("America/New_York")).isoformat()

    conn = get_db_connection()
    create_subjects_table(conn)
    cur = conn.execute(
        """
        INSERT INTO subjects
            (qualtrics_id, condition, game_type, draw_is_take, ts_welcome)
        VALUES (?,?,?,?,?)
        """,
        (qualtrics_id, str(condition), str(game_type), draw_is_take, ts)
    )
    conn.commit()
    new_id = cur.lastrowid
    conn.close()

    return jsonify(subject_id=new_id)

@app.route("/final_round_update", methods=["POST"])
def final_round_update():
    rec = request.get_json(force=True) or {}
    sid = rec.get("subject_id")
    chosen = rec.get("final_chosen_round")

    if not sid or not chosen:
        return jsonify(ok=False, error="subject_id or final_chosen_round missing"), 400

    conn = get_db_connection()
    create_actions_table(conn)

    conn.execute(
        "UPDATE game_actions SET round_implemented = NULL WHERE subject_id = ?",
        (sid,)
    )
    conn.execute(
        """
        UPDATE game_actions
           SET round_implemented = 1
         WHERE subject_id = ?
           AND round_number = ?
        """,
        (sid, chosen)
    )
    conn.commit()
    conn.close()
    return jsonify(ok=True)

@app.route("/subject_update", methods=["POST"])
def subject_update():
    rec   = request.get_json()
    sid = rec.get("subject_id")
    stage = rec.get("stage")

    if not sid:
        return jsonify(error="subject_id missing"), 400

    conn = get_db_connection()
    create_subjects_table(conn)
    ts   = datetime.now(ZoneInfo("America/New_York")).isoformat()

    if stage == "instructions":
        conn.execute(
            "UPDATE subjects SET ts_instructions=? WHERE subject_id=?",
            (ts, sid)
        )

    elif stage == "demo":
        conn.execute(
            "UPDATE subjects SET ts_demo=? WHERE subject_id=?",
            (ts, sid)
        )

    elif stage == "check":
        attempts  = rec.get("attempts")
        f1, f2, f3, f4 = rec.get("first_try_results", [None]*4)
        pass_flag = rec.get("all_first_try")
        conn.execute(
            """
            UPDATE subjects SET
              ts_check=?,
              quiz_attempts=?,
              check_first_try1=?, check_first_try2=?, check_first_try3=?, check_first_try4=?,
              check_first_try_pass=?
            WHERE subject_id=?
            """,
            (ts, attempts, f1, f2, f3, f4, pass_flag, sid)
        )

    elif stage == "demo2":
        conn.execute(
            "UPDATE subjects SET ts_demo2=? WHERE subject_id=?",
            (ts, sid)
        )

    elif stage == "check2":
        attempts  = rec.get("attempts")
        f1, f2, f3, f4 = rec.get("first_try_results", [None]*4)
        pass_flag = rec.get("all_first_try")
        conn.execute(
            """
            UPDATE subjects SET
              ts_check2=?,
              quiz2_attempts=?,
              check2_first_try1=?, check2_first_try2=?, check2_first_try3=?, check2_first_try4=?,
              check2_first_try_pass=?
            WHERE subject_id=?
            """,
            (ts, attempts, f1, f2, f3, f4, pass_flag, sid)
        )

    elif stage == "game":
        conn.execute(
            "UPDATE subjects SET ts_game=? WHERE subject_id=?",
            (ts, sid)
        )

    elif stage == "end":
        conn.execute(
            "UPDATE subjects SET ts_end=? WHERE subject_id=?",
            (ts, sid)
        )

    else:
        conn.close()
        return jsonify(error="unknown stage"), 400

    conn.commit()
    conn.close()
    return jsonify(status="ok")

@app.route("/round", methods=["POST"])
def round_event():
    rec = request.get_json(force=True)

    conn = get_db_connection()
    create_actions_table(conn)

    base_cols = [
        "subject_id",
        "action_game_type",
        "action_condition",
        "round_number",
        "round_grp_number",
        "n_blobs",
        "draw_is_take",
        "round_start_time",
        "move1", "timestamp1",
        "move2", "timestamp2",
        "move3", "timestamp3",
        "move4", "timestamp4",
        "move5", "timestamp5",
        "move6", "timestamp6",
        "cum_score_you",
        "cum_score_opp",
        "cum_score_obj",
        "round_payoff",
        "round_implemented",
        "draw_mode",
        "balanced_cats",
    ]

    all_cols = base_cols + CSV_COLUMNS

    vals = [rec.get(c) for c in all_cols]

    placeholders = ",".join(["?"] * len(all_cols))
    colnames = ",".join(all_cols)

    try:
        conn.execute(
            f"INSERT INTO game_actions ({colnames}) VALUES ({placeholders})",
            vals
        )
        conn.commit()
    except Exception as e:
        print("ROUND INSERT ERROR:", e)
        return jsonify({"ok": False, "error": str(e)}), 500
    finally:
        conn.close()

    return jsonify({"ok": True})

@app.route("/get_payoff", methods=["GET"])
def get_payoff():
    """Bonus for the randomly implemented round; Qualtrics calls this with ?qualtrics_id=<ResponseID>."""
    response_id = request.args.get("qualtrics_id", None)
    if not response_id:
        return jsonify(round_payoff=0.0, error="qualtrics_id missing"), 400

    conn = get_db_connection()
    create_subjects_table(conn)
    create_actions_table(conn)

    subj_row = conn.execute(
        "SELECT subject_id FROM subjects WHERE qualtrics_id = ? LIMIT 1",
        (response_id,)
    ).fetchone()

    if not subj_row:
        conn.close()
        return jsonify(round_payoff=0.0, error="qualtrics_id not found"), 200

    subject_id = subj_row[0]

    payoff_row = conn.execute(
        """
        SELECT round_payoff
          FROM game_actions
         WHERE subject_id = ?
           AND round_implemented = 1
         LIMIT 1
        """,
        (subject_id,)
    ).fetchone()
    conn.close()

    if not payoff_row:
        return jsonify(round_payoff=0.0, error="round_implemented not set yet"), 200

    return jsonify(round_payoff=float(payoff_row[0] or 0.0))


if __name__ == "__main__":
    with get_db_connection() as _c:
        create_subjects_table(_c)
        create_actions_table(_c)
    app.run(debug=True, host="0.0.0.0", port=8080)