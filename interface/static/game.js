const cfg = JSON.parse(document.getElementById("config").textContent);
const {
  STAGES_ROUNDS,
  STAGES_BLOBS,
  STAGES_GAME_TYPES,
  WITHIN_SUBJECT,
  ROUND_SAMPLES,
  ROUND_CONFIG,
  CSV_COLUMNS,
  NUM_CATEGORIES,
  BASE_PAYOFF_ADV,
  BASE_PAYOFF_COOP,
  VARIABLE_PAYOFF,
  GAME_TYPE,
  DRAW_IS_TAKE,
  BALANCED_CATS,
  ASSIGNMENT, 
  QUALTRICS_ID
} = cfg;
const ENABLE_ANIMATIONS = false;
const STEP_TELEPORT_DELAY = 500; 
const HUMAN_TO_FIRST_AUTO_DELAY = 1000;   // player → first CPU pick
const AUTO_TO_AUTO_DELAY        = 500;   // CPU ↔ CPU (or auto-player)
const LAST_AUTO_TO_POPUP_DELAY  = 1500;   // final pick → endRound()

/*  ── grab the Qualtrics ResponseID from the query-string once ── */
const urlParams     = new URLSearchParams(window.location.search);
const qualtricsID   = urlParams.get("qualtrics_id") || "";

let currentStage   = 0;
let transitionDone = false;
const roundsPerStage = STAGES_ROUNDS[0];
const totalStages    = STAGES_ROUNDS.length;
const totalRounds    = STAGES_ROUNDS.reduce((a,b)=>a+b, 0);
let stageRound = 1;

const POD_SEQUENCES = {
  2 : [ ["A","L"], ["B","R"] ],                       // 2 pods
  4 : [ ["B","L"],["D","R"],["A","L"],["C","R"] ],    // 4 pods
  6 : [ ["E","L"],["C","R"],["B","L"],["D","R"],["F","L"],["A","R"] ]
};
// Letter→(red,blue) lookup table
const DEMO_PODS = { A:[2,2], B:[3,3], C:[4,4], D:[5,5], E:[6,6], F:[7,7] };

const QUIZ_POD_SEQUENCES = {
    2 : [ ["B","L"], ["A","R"] ],                       // 2 pods
    4 : [ ["C","L"],["B","R"],["A","L"],["D","R"] ],    // 4 pods
    6 : [ ["F","L"],["B","R"],["D","L"],["C","R"],["A","L"],["E","R"] ]
  };

function stagePods(i = currentStage){
  return Number(STAGES_BLOBS[i]);   // force numeric so 6/4/2 match your sequence keys
}        // 2 | 4 | 6
function stageGameType(i = currentStage){
  return STAGES_GAME_TYPES[i];        // 'adversarial' | 'cooperative'
}

function rotateToFirstSide(seq, wantSide){
  if (!Array.isArray(seq) || !seq.length) return seq;
  const k = seq.findIndex(pair => pair && pair[1] === wantSide);
  if (k <= 0) return seq;               // already starts correctly (or not found)
  return seq.slice(k).concat(seq.slice(0, k));
}

/* ───────────────── SCREEN MANAGEMENT (0 Welcome, 1 Instructions, 2 Game) ─ */
let subjectID = null;
let quizAttempts    = 0;
let firstTryCorrect = [null, null, null, null];
let quizInfoBundle = { quiz: null, quiz2: null };

function nowNY() {
  return new Date().toLocaleString("en-US", { timeZone: "America/New_York" });
}

function postQuizInfo(which /* "quiz" | "quiz2" */, payload) {
  try {
    quizInfoBundle[which] = payload;
    const delta = {};
    delta[which] = payload;

    window.parent.postMessage(
      { type: "QUIZ_INFO", quiz_info: delta },
      "*"
    );
  } catch (e) {
    console.warn("postQuizInfo failed", e);
  }
}
function showScreen(i){
  document.querySelectorAll(".screen").forEach((s,idx)=>
    s.classList.toggle("active", idx===i));

    if (i === 2) {
    const narr = document.getElementById("demoNarration");
    if (narr) narr.style.display = "none";
  }

  if (i === 3) resetQuizPlayButtonForStage();
}

function replacePartnerWords(html, who) {
  const lower = who.toLowerCase();  // "teammate" or "opponent"
  const upper = lower.toUpperCase(); // "TEAMMATE" or "OPPONENT"
  const title = who;                 // "Teammate" or "Opponent"

  return html.replace(/teammate|opponent/gi, (match) => {
    if (match === match.toUpperCase()) {
      // TEAMMATE / OPPONENT
      return upper;
    } else if (match[0] === match[0].toUpperCase()) {
      // Teammate / Opponent
      return title;
    } else {
      // teammate / opponent
      return lower;
    }
  });
}

function setPartnerLabels(stage = currentStage) {
  const gt = STAGES_GAME_TYPES[stage];
  const who = gt === "cooperative" ? "Teammate" : "Opponent";
  const whoLower = who.toLowerCase();

  // Update the side box labels
  const demoRightLabel = document.getElementById("demoRightLabel");
  if (demoRightLabel) demoRightLabel.textContent = who;

  const quizRightLabel = document.getElementById("quizRightLabel");
  if (quizRightLabel) quizRightLabel.textContent = who;

  // Update right score title on the game screen
  const rightScoreTitle = document.querySelector('#right-area .score-title');
  if (rightScoreTitle) {
    rightScoreTitle.textContent = `${who}'s Score`;
  }

  // 🔹 NEW: update the GAME screen helper text (#instructions)
  const gameInstructions = document.getElementById("instructions");
  if (gameInstructions) {
    gameInstructions.innerHTML = replacePartnerWords(gameInstructions.innerHTML, who);
  }

  // 🔹 NEW: update the INSTRUCTIONS PAGE text (#instructionsText)
  const introInstructions = document.getElementById("instructionsText");
  if (introInstructions) {
    introInstructions.innerHTML = replacePartnerWords(introInstructions.innerHTML, who);
  }

  // Update metric label on the scoreDifference element
  const scoreDiff = document.getElementById("scoreDifference");
  if (scoreDiff) {
    const metricLabel = gt === "cooperative" ? "Score Sum: " : "Score Difference: ";
    const parts = scoreDiff.textContent.split(":");
    const value = parts.length > 1 ? parts.slice(1).join(":").trim() : "0";
    scoreDiff.textContent = metricLabel + value;
  }
}

function renderQuizForm(redSum, blueSum, gtThisStage) {
  const form = document.getElementById("quizFormContainer");

  const partnerWord = gtThisStage === "cooperative" ? "teammate" : "opponent";

  const q1Label = `How many <span style='color:red;font-weight:bold;'>red</span> dots did you get?`;
  const q2Label = `How many <span style='color:blue;font-weight:bold;'>blue</span> dots did your ${partnerWord} get?`;

  form.innerHTML = `
    <div class="quiz-row">
      <label>${q1Label}</label>
      <input id="q1" type="number" style="width:60px;margin-left:10px">
      <span id="f1" class="feedback"></span>
    </div>

    <div class="quiz-row">
      <label>${q2Label}</label>
      <input id="q2" type="number" style="width:60px;margin-left:10px">
      <span id="f2" class="feedback"></span>
    </div>

    <div class="quiz-row">
      <label>What is the ${gtThisStage==="cooperative"?"Score Sum":"Score Difference"}?</label>
      <input id="q3" type="number" style="width:60px;margin-left:10px">
      <span id="f3" class="feedback"></span>
    </div>

    <div class="quiz-row">
      <label>Calculate the bonus for this ${gtThisStage==="cooperative"?"Score Sum":"Score Difference"}.</label>
      <input id="q4" type="number" step="0.01" style="width:60px;margin-left:10px">
      <span id="f4" class="feedback"></span>
    </div>

    <button id="btnCheckQuiz" style="margin-top:10px;">Check answers</button>
    <div id="quizTryAgain" style="color:red;margin-top:5px;"></div>
  `;

  document.getElementById("btnCheckQuiz").onclick =
    () => checkQuizAnswers(redSum, blueSum, gtThisStage);
}

function checkQuizAnswers(redSum, blueSum, gtThisStage) {
  quizAttempts++;
  const obj   = gtThisStage === "cooperative" ? redSum + blueSum : redSum - blueSum;
  const base  = gtThisStage === "adversarial" ? BASE_PAYOFF_ADV : BASE_PAYOFF_COOP;
  const payoff= +(base + VARIABLE_PAYOFF * obj).toFixed(2);

  const expected = { 1: redSum, 2: blueSum, 3: obj, 4: payoff };

  if(quizAttempts === 1){
    for(let i=1;i<=4;i++){
      const val = parseFloat(document.getElementById(`q${i}`).value.trim());
      firstTryCorrect[i-1] = (val === expected[i]) ? 1 : 0;
    }
  }

  let allCorrect = true;
  document.getElementById("quizTryAgain").textContent = "";
  for(let i=1;i<=4;i++){
    const userVal = parseFloat(document.getElementById(`q${i}`).value.trim());
    const fb = document.getElementById(`f${i}`);
    fb.textContent = userVal === expected[i] ? "✅" : "❌";
    if(userVal !== expected[i]) allCorrect = false;
  }

  if(!allCorrect){
    document.getElementById("quizTryAgain").textContent = "Try again.";
    return;
  }

    /* success: record stats, swap button to Continue */
    const isFirst = (currentStage === 0);
    const route   = isFirst ? "/subject_update" : "/stage_update";
    const stage   = isFirst ? "check" : "check2";

    // ---- 1) Send bundle to Qualtrics (one ED: quiz_info) ----
    const whichQuiz = isFirst ? "quiz" : "quiz2";
    const quizPayload = {
      subject_id: subjectID,
      stage: whichQuiz,                 // "quiz" or "quiz2" (consistent)
      attempts: quizAttempts,
      first_try_results: firstTryCorrect.slice(), // [0/1,...] for q1-q4
      all_first_try: (quizAttempts === 1 ? 1 : 0),
      completed_at: nowNY()
    };
    postQuizInfo(whichQuiz, quizPayload);

    // ---- 2) Keep your server logging (unchanged) ----
    fetch(route, {
      method:"POST",
      headers:{"Content-Type":"application/json"},
      body: JSON.stringify({
        subject_id: subjectID,
        stage,
        attempts: quizAttempts,
        first_try_results: firstTryCorrect,
        all_first_try: (quizAttempts === 1 ? 1 : 0)
      })
    });

  const btn = document.getElementById("btnCheckQuiz");
  btn.textContent = "Continue";
  btn.onclick = () => {
    if(isFirst){
      fetch("/subject_update",{
        method:"POST",headers:{"Content-Type":"application/json"},
        body:JSON.stringify({subject_id:subjectID,stage:"game"})
      }).finally(()=>{ showScreen(4); initGame(); });
    }else{
      fetch("/subject_update",{
        method:"POST",headers:{"Content-Type":"application/json"},
        body:JSON.stringify({subject_id:subjectID,stage:"game"})
      }).finally(()=>{ showScreen(4); initGame(); });
    }
  };
}

  function centreContainer(el){
    const r = el.getBoundingClientRect();
    el.style.position   = "absolute";
    el.style.left       = "50%";
    el.style.top        = "50%";
    el.style.transform  = "translate(-50%,-50%)";
    /* make sure parent has position context */
    if (getComputedStyle(el.parentElement).position === "static"){
        el.parentElement.style.position = "relative";
    }
}

// 0 ➜ 1  (Welcome ➜ Instructions)
document.getElementById("btnBeginWelcome").onclick = () => {
  fetch("/subject_new", {
    method: "POST",
    headers: { "Content-Type":"application/json" },
    body: JSON.stringify({
      qualtrics_id: qualtricsID || QUALTRICS_ID || "",
      condition: ASSIGNMENT.condition,
      game_type: ASSIGNMENT.game_type,
      draw_is_take: ASSIGNMENT.draw_is_take
    })
  })
  .then(r => r.json())
  .then(d => { subjectID = d.subject_id; showScreen(1); })
  .catch(console.error);
};
  
  // 1 ➜ 2  (Instructions ➜ Demo)
  document.getElementById("btnInstrContinue").onclick = () => {
    fetch("/subject_update",{
        method:"POST",
        headers:{"Content-Type":"application/json"},
        body:JSON.stringify({subject_id:subjectID, stage:"instructions"})
      })
      .then(()=>{ showScreen(2); })
      .catch(console.error);
    setPartnerLabels();
  };
  
// Modify the setupDemoPods function to ensure pods start in the center center
function setupDemoPods () {
  const podCount      = stagePods();         // ← dynamic
  const demoContainer = document.getElementById("demoStaticPods");
  demoContainer.innerHTML = "";              // clear previous stage

  /* layout coordinates */
  const layout = (podCount === 2)
        ? [[0,0],[1,0]]
        : (podCount === 4)
            ? [[0,0],[0,1],[1,0],[1,1]]
            : [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]];  // 6-pod
  const spacing = 150;
  const cols = Math.max(...layout.map(l => l[1])) + 1;
  const rows = Math.max(...layout.map(l => l[0])) + 1;

  demoContainer.style.cssText =
      `position:relative;margin:10px auto;width:${cols*spacing}px;height:${rows*spacing}px;`;

  const inner = document.createElement("div");
  /* responsive flex grid instead of a fixed-width block */
  inner.style.cssText =
      `display:flex; flex-wrap:wrap; justify-content:center; gap:10px;
      position:relative; margin:0 auto; max-width:100%;`;
  demoContainer.appendChild(inner);


  for (let i = 0; i < podCount; i++) {
    const letter = String.fromCharCode(65 + i);
    const [red, blue] = DEMO_PODS[letter];

    const pod = document.createElement("div");
    pod.className = "blob-container";
    pod.dataset.letter = letter;
    pod.style.cssText  = "position:absolute;width:100px;height:100px;";

    const L = document.createElement("div");
    L.className = "blob-circles-left";  L.style.width = "40%";
    for (let j = 0; j < red; j++)
      L.appendChild(document.createElement("div")).classList.add("circle","red");
    pod.appendChild(L);

    const R = document.createElement("div");
    R.className = "blob-circles-right"; R.style.width = "40%";
    for (let j = 0; j < blue; j++)
      R.appendChild(document.createElement("div")).classList.add("circle","blue");
    pod.appendChild(R);

    const [r, c] = layout[i];
    pod.style.left = c * spacing + "px";
    pod.style.top  = r * spacing + "px";
    inner.appendChild(pod);
  }
}

let demoIsRunning = false;
function playDemo () {
  if (demoIsRunning) return;        // prevent double-click reentry
  demoIsRunning = true;

  const gtThisStage = stageGameType();
  const ROLE_S = gtThisStage === "cooperative" ? "teammate" : "opponent";
  const podCount = stagePods();

  let seq = POD_SEQUENCES[podCount];
  if (!seq) {
    console.error("No POD_SEQUENCES entry for podCount=", podCount, POD_SEQUENCES);
    demoIsRunning = false;
    return;
  }

  // GIVE mode: rotate draw ORDER only (keeps same pod->side mapping)
  const giveMode = (!DRAW_IS_TAKE);
  seq = rotateToFirstSide(seq, giveMode ? "R" : "L");

  const mid   = document.getElementById("demoStaticPods");
  const leftG = document.getElementById("demoLeftGrid");
  const rightG= document.getElementById("demoRightGrid");
  const narr  = document.getElementById("demoNarration");

  leftG.innerHTML = "";
  rightG.innerHTML = "";
  narr.textContent = "";
  narr.style.display = "none";

  const elByLetter = {};
  mid.querySelectorAll(".blob-container")
     .forEach(el => { elByLetter[el.dataset.letter] = el; });

  let me = 0, opp = 0, step = 0;

  function moveNext(){
    if (step >= seq.length){
      demoIsRunning = false;
      showNarration();
      return;
    }

    const item = seq[step++];           // defensive: avoid destructuring undefined
    if (!item) {
      console.warn("Demo sequence ended unexpectedly; forcing narration.");
      demoIsRunning = false;
      showNarration();
      return;
    }
    const [letter, side] = item;

    const pod   = elByLetter[letter];
    if (!pod) {
      console.warn("Missing pod element for letter=", letter, elByLetter);
      setTimeout(moveNext, STEP_TELEPORT_DELAY);
      return;
    }

    const grid  = (side === "L") ? leftG : rightG;
    const [redCnt, blueCnt] = DEMO_PODS[letter];

    // NO-ANIMATION branch
    const final = pod.cloneNode(true);
    final.className = "blob-container-static";
    final.style.cssText = "width:100px;height:100px;margin:20px auto;";
    grid.appendChild(final);
    pod.remove();

    if (side === "L") me += redCnt; else opp += blueCnt;
    setTimeout(moveNext, STEP_TELEPORT_DELAY);
  }

  function showNarration(){
    const base   = gtThisStage === "adversarial" ? BASE_PAYOFF_ADV : BASE_PAYOFF_COOP;
    const objVal = gtThisStage === "cooperative" ? me + opp : me - opp;
    const bonus  = (base + VARIABLE_PAYOFF * objVal).toFixed(2);

    const lines = [
      `You got ${me} <span class="redText">red</span> dots, so you scored ${me} points.`,
      `Your ${ROLE_S} got ${opp} <span class="blueText">blue</span> dots, so they scored ${opp} points.`,
    ];

    lines.push(
      `${gtThisStage === "cooperative" ? "Score Sum" : "Score Difference"} = ${objVal}.`,
      `${objVal} × $${VARIABLE_PAYOFF.toFixed(2)}${gtThisStage === "adversarial" ? ` + $${base.toFixed(2)}` : ""} = $${bonus}`,
      `This corresponds to a bonus of $${bonus}.`,
      `Click Continue to check your understanding.`
    );

    narr.style.display = "block";
    narr.innerHTML = "";
    lines.forEach(t => {
      const p = document.createElement("p");
      p.style.display = "none";
      p.innerHTML = t;
      narr.appendChild(p);
    });

    let idx = 0;
    const btn = document.getElementById("btnDemoContinue");
    btn.disabled = true;
    (function reveal(){
      if (idx < narr.children.length){
        narr.children[idx++].style.display = "block";
        setTimeout(reveal, 2500);
      } else {
        btn.disabled = false;
      }
    })();
  }

  moveNext();
}

function setupQuizPods() {
  // Clear previous quiz content
  document.getElementById("quizLeftGrid").innerHTML = "";
  document.getElementById("quizRightGrid").innerHTML = "";
  document.getElementById("quizFormContainer").innerHTML = "";
  
  const narr = document.getElementById("quizNarration");
  if (narr) {
    narr.innerHTML = "";
    narr.style.display = "none";
  }
  
  // Clear and recreate the quiz static pods container
  const podCount = stagePods();
  const quizContainer = document.getElementById("quizStaticPods");
  quizContainer.innerHTML = "";
  
  // Define layout based on pod count
  const layout = (podCount === 2)
    ? [[0,0],[1,0]]
    : (podCount === 4)
      ? [[0,0],[0,1],[1,0],[1,1]]
      : [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]];
  
  const spacing = 150;
  const cols = Math.max(...layout.map(l => l[1])) + 1;
  const rows = Math.max(...layout.map(l => l[0])) + 1;
  
  quizContainer.style.cssText = `position:relative;margin:0 auto;width:${cols*spacing}px;height:${rows*spacing}px;`;
  
  // Create the inner container for proper centering
  const inner = document.createElement("div");
  inner.style.cssText = `position:absolute;top:0;left:50%;transform:translateX(-50%);width:${cols*spacing}px;height:${rows*spacing}px;`;
  quizContainer.appendChild(inner);
  
  // Create each pod
  for (let i = 0; i < podCount; i++) {
    const letter = String.fromCharCode(65 + i);
    const [red, blue] = DEMO_PODS[letter];
    
    const pod = document.createElement("div");
    pod.className = "blob-container";
    pod.dataset.letter = letter;
    pod.style.cssText = "position:absolute;width:100px;height:100px;";
    
    const L = document.createElement("div");
    L.className = "blob-circles-left";
    L.style.width = "40%";
    for (let j = 0; j < red; j++) {
      L.appendChild(document.createElement("div")).classList.add("circle", "red");
    }
    pod.appendChild(L);
    
    const R = document.createElement("div");
    R.className = "blob-circles-right";
    R.style.width = "40%";
    for (let j = 0; j < blue; j++) {
      R.appendChild(document.createElement("div")).classList.add("circle", "blue");
    }
    pod.appendChild(R);
    
    const [r, c] = layout[i];
    pod.style.left = c * spacing + "px";
    pod.style.top = r * spacing + "px";
    inner.appendChild(pod);
  }
  
  // Make sure the quiz play button is enabled
  const btnQuizPlay = document.getElementById("btnQuizPlay");
  if (btnQuizPlay) {
    btnQuizPlay.disabled = false;
  }
}

let quizIsRunning = false;
function playQuizDemo() {
    quizAttempts = 0;
    firstTryCorrect = [null, null, null, null];
  if (quizIsRunning) return;        // prevent double-start
  quizIsRunning = true;

  const gtThisStage = stageGameType();
  const podCount = stagePods();

  let seq = QUIZ_POD_SEQUENCES[podCount];
  if (!seq) {
    console.error("No QUIZ_POD_SEQUENCES entry for podCount=", podCount, QUIZ_POD_SEQUENCES);
    quizIsRunning = false;
    return;
  }

  // GIVE mode: rotate ORDER only
  const giveMode = (!DRAW_IS_TAKE);
  seq = rotateToFirstSide(seq, giveMode ? "R" : "L");

  const mid   = document.getElementById("quizStaticPods");
  const leftG = document.getElementById("quizLeftGrid");
  const rightG= document.getElementById("quizRightGrid");
  let narr = document.getElementById("quizNarration");
  if (!narr){
    narr = document.createElement("div");
    narr.id = "quizNarration";
    mid.after(narr);
  }

  leftG.innerHTML = ""; rightG.innerHTML = "";
  narr.innerHTML = ""; narr.style.display = "none";

  const elByLetter = {};
  mid.querySelectorAll(".blob-container")
     .forEach(el => { elByLetter[el.dataset.letter] = el; });

  let me = 0, opp = 0, step = 0;

  function moveNext(){
    if (step >= seq.length){
      quizIsRunning = false;
      renderQuizForm(me, opp, gtThisStage);
      return;
    }

    const item = seq[step++];
    if (!item) {
      console.warn("Quiz sequence ended unexpectedly; forcing quiz form.");
      quizIsRunning = false;
      renderQuizForm(me, opp, gtThisStage);
      return;
    }
    const [letter, side] = item;

    const pod  = elByLetter[letter];
    if (!pod){
      console.warn("Missing pod in quiz for letter=", letter);
      setTimeout(moveNext, STEP_TELEPORT_DELAY);
      return;
    }

    const grid = (side === "L") ? leftG : rightG;
    const [rCnt, bCnt] = DEMO_PODS[letter];

    const final = pod.cloneNode(true);
    final.className = "blob-container-static";
    final.style.cssText = "width:100px;height:100px;margin:20px auto;";
    grid.appendChild(final);
    pod.remove();

    if (side === "L") me += rCnt; else opp += bCnt;
    setTimeout(moveNext, STEP_TELEPORT_DELAY);
  }

  moveNext();
}

/* ───────────────────────────────── GAME LOGIC  (unchanged) ───────────────── */
let currentRound=1, gameLog=[], playerScore=0, opponentScore=0;
let playerTurn=true, selectionAllowed=true, blobs=[], blobSize=100;
let moves=[], moveTimestamps=[], roundStartTime=null, finalChosenRound=null;
let autoModeRound = false;

const roundNumberEl   = document.getElementById("roundNumber");
const playerScoreEl   = document.getElementById("playerScore");
const opponentScoreEl = document.getElementById("opponentScore");
const turnIndicatorEl = document.getElementById("turnIndicator");
const middleGridEl    = document.getElementById("middle-grid");
const selectedLeftEl  = document.getElementById("selected-left");
const selectedRightEl = document.getElementById("selected-right");
const modalOverlayEl  = document.getElementById("modal-overlay");
const modalMessageEl  = document.getElementById("modal-message");
const modalButtonEl   = document.getElementById("modal-button");
const totalRoundsEl   = document.getElementById("totalRounds");

function initGame () {
  /* ── compute the GLOBAL round number every time a new Game stage starts ── */
  if (currentRound === 0 || stageRound === 1) {
      currentRound = currentStage * roundsPerStage + 1;
  }

  stageRound = 1;                           // within-stage counter
  totalRoundsEl.textContent = roundsPerStage;

  updateGameScreenInstructions();
  initRound();
}

// New function to update game screen instructions
function updateGameScreenInstructions() {
  const gtThisStage = stageGameType();
  const partner = gtThisStage === "cooperative" ? "Teammate" : "Opponent";
  const partnerLower = partner.toLowerCase();
  
  // Update the condition-specific text
  const metric = gtThisStage === "cooperative" 
    ? "Score Sum = Your Score + Teammate Score."
    : "Score Difference = Your Score – Opponent Score.";
  
  const basePayoff = gtThisStage === "adversarial" ? BASE_PAYOFF_ADV : BASE_PAYOFF_COOP;
  const payoffText = gtThisStage === "adversarial"
    ? `Your bonus = ($${VARIABLE_PAYOFF.toFixed(2)} × Score Difference) + $${basePayoff.toFixed(2)}.`
    : `Your bonus = $${VARIABLE_PAYOFF.toFixed(2)} × Score Sum.`;
  const payoffTextSum = gtThisStage === "adversarial"
    ? `YOUR RED DOTS minus OPPONENT'S BLUE DOTS determines BONUS.`
    : `YOUR RED DOTS plus TEAMMATE'S BLUE DOTS determines BONUS.`;
  
  // Get pod count for this stage
  const podCount = stagePods();
  
  // Determine rules text based on pod count
  let rulesText;
  if (podCount === 2) {
    rulesText = DRAW_IS_TAKE
      ? `In each round, you will choose the first pod and take it for yourself.<br>
        Then your ${partnerLower} will get the remaining pod.`
      : `In each round, you will choose the first pod and give it to your ${partnerLower}.<br>
        Then you will get the remaining pod.`;
  } else {
    rulesText = DRAW_IS_TAKE
      ? `In each round, you will choose the first pod and take it for yourself.<br>` +
        `Then remaining pods will be drawn at random, taking turns between your ${partnerLower} and you. ` +
        `You will each end up with ${podCount/2} pods.`
      : `In each round, you will choose the first pod and give it to your ${partnerLower}.<br>` +
        `Then remaining pods will be drawn at random, taking turns between you and your ${partnerLower}. ` +
        `You will each end up with ${podCount/2} pods.`;
  }

  
  // Update the instructions HTML
  const instructionsEl = document.getElementById('instructions');
  const giveTakeHTML = DRAW_IS_TAKE
  ? `Every time you get a pod, you get points equal to its 
     <span style='color:red;font-weight:bold;'>red</span> dots, and your ${partnerLower} gets nothing.<br>
     Every time your ${partnerLower} gets a pod, they get points equal to its 
     <span style='color:blue;font-weight:bold;'>blue</span> dots, and you get nothing.<br>`
  : `Every time your ${partnerLower} gets a pod, they get points equal to its 
     <span style='color:blue;font-weight:bold;'>blue</span> dots, and you get nothing.<br>
     Every time you get a pod, you get points equal to its 
     <span style='color:red;font-weight:bold;'>red</span> dots, and your ${partnerLower} gets nothing.<br>`;
  if (instructionsEl) {
    instructionsEl.innerHTML = `
      ${rulesText}<br>
      ${giveTakeHTML}<br>
      <span style='font-weight:bold;'>${metric}<br>${payoffText}</span>
    `;
  }
  
  // Update the right score title
  const rightScoreTitle = document.querySelector('#right-area .score-title');
  if (rightScoreTitle) {
    rightScoreTitle.textContent = `${partner}'s Score`;
  }
  
  // Update score difference/sum label
  updateScoreObj();
}

function initRound(){
    const roundIndex = currentRound-1;
    autoModeRound = (ROUND_CONFIG[roundIndex].blobCount > 2);  // 6‑pod ⇒ auto
    playerTurn = true;  selectionAllowed = true;
    moves=[]; moveTimestamps=[];
    roundStartTime = new Date().toLocaleString('en-US',{timeZone:'America/New_York'});
    moves = [];
    moveTimestamps = [];
    roundStartTime = new Date().toLocaleString('en-US', {timeZone:'America/New_York'});
    playerScore   = 0;
    opponentScore = 0;

    roundNumberEl.textContent = stageRound;
    playerScoreEl.textContent = playerScore;
    opponentScoreEl.textContent = opponentScore;
    updateScoreObj();
  
    middleGridEl.innerHTML   = "";
    selectedLeftEl.innerHTML = "";
    selectedRightEl.innerHTML= "";
  
    computeBlobSize();
    generateBlobsForRound(currentRound);
    displayBlobs();
    updateTurnIndicator();
  }
  
  
  function updateScoreObj() {
    const gt = stageGameType();
    const val = (gt === "cooperative") ? playerScore + opponentScore : playerScore - opponentScore;
    const lab = (gt === "cooperative") ? "Score Sum: " : "Score Difference: ";
    document.getElementById("scoreDifference").textContent = lab + val;
  }

  function playerAutoPick(){
    const unsel = blobs.filter(b=>!b.selected);
    if(!unsel.length){
      setTimeout(endRound, LAST_AUTO_TO_POPUP_DELAY);
      return;
    }
    const b = unsel[Math.floor(Math.random()*unsel.length)];
    recordMove(b.id); b.selected=true;
    animateBlobToSide(b,true,()=>{
         playerScore+=b.redCount; playerScoreEl.textContent=playerScore;
         updateScoreObj(); setTimeout(opponentPick, AUTO_TO_AUTO_DELAY);
    });
  }
  
  function schedulePlayerAuto(){           // small wrapper for clarity
    setTimeout(playerAutoPick, AUTO_TO_AUTO_DELAY);
  }

function computeBlobSize() {
    let midWidth = document.getElementById("middle-area").offsetWidth;
    let margin = 20;
    let size = Math.floor(midWidth / 4) - margin;
    blobSize = size < 50 ? 50 : size;
    document.documentElement.style.setProperty('--blob-size', blobSize + 'px');
}

function generateBlobsForRound(r) {
    let roundIndex = r - 1;
    let countBlobs = ROUND_CONFIG[roundIndex].blobCount;
    let sample = ROUND_SAMPLES[roundIndex];
    blobs = [];
    for(let i = 0; i < countBlobs; i++){
    let letter = String.fromCharCode(65 + i);
    let redCount = parseInt(sample[letter + "1"]);
    let blueCount = parseInt(sample[letter + "2"]);
    blobs.push({
        id: i,
        letter: letter,
        redCount: redCount,
        blueCount: blueCount,
        selected: false,
        posX: 0,
        posY: 0,
        element: null
    });
    }
    setPositionsForBlobs(countBlobs);
}

function setPositionsForBlobs(T) {
    let layout = [];
    if(T===2) { layout = [[0,0],[1,0]]; }
    else if(T===4) { layout = [[0,0],[0,1],[1,0],[1,1]]; }
    else if(T===6) { layout = [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]]; }
    else if(T===8) { layout = [[0,0],[0,1],[0,2],[0,3],[1,0],[1,1],[1,2],[1,3]]; }
    else { for(let i = 0; i < T; i++) layout.push([0, i]); }
    let colSpacing = blobSize + 10;
    let rowSpacing = blobSize + 10;
    let maxCol = Math.max(...layout.map(l => l[1])) + 1;
    let layoutWidth = maxCol * colSpacing;
    let midW = document.getElementById("middle-grid").offsetWidth;
    let offsetLeft = layoutWidth < midW ? Math.floor((midW - layoutWidth)/2) : 0;
    for(let i = 0; i < T; i++){
    let r = layout[i][0], c = layout[i][1];
    blobs[i].posX = offsetLeft + c * colSpacing;
    blobs[i].posY = r * rowSpacing;
    }
}

function displayBlobs() {
    middleGridEl.innerHTML = "";
    for(let b of blobs){
    if(b.selected) continue;
    let div = document.createElement("div");
    div.classList.add("blob-container");
    div.style.width = blobSize + "px";
    div.style.height = blobSize + "px";
    div.style.left = b.posX + "px";
    div.style.top = b.posY + "px";
    let leftDiv = document.createElement("div");
    leftDiv.className = "blob-circles-left";
    leftDiv.style.width = "40%";
    for(let i = 0; i < b.redCount; i++){
        let c = document.createElement("div");
        c.classList.add("circle", "red");
        leftDiv.appendChild(c);
    }
    div.appendChild(leftDiv);
    let rightDiv = document.createElement("div");
    rightDiv.className = "blob-circles-right";
    rightDiv.style.width = "40%";
    for(let i = 0; i < b.blueCount; i++){
        let c = document.createElement("div");
        c.classList.add("circle", "blue");
        rightDiv.appendChild(c);
    }
    div.appendChild(rightDiv);
    if(playerTurn) {
        let btnC = document.createElement("div");
        btnC.className = "select-button-container";
        let btn = document.createElement("button");
        btn.className = "select-button";
        btn.textContent = "Select";
        btn.onclick = () => {
        if(selectionAllowed && playerTurn){
            recordMove(b.id);
            playerSelectBlob(b.id);
        }
        };
        btnC.appendChild(btn);
        div.appendChild(btnC);
    }
    b.element = div;
    middleGridEl.appendChild(div);
    }
}

function updateTurnIndicator(){
  if (!playerTurn) {
    turnIndicatorEl.textContent = (stagePods() === 2)
      ? "...REMAINING DRAW..."
      : "...RANDOM DRAWS...";
    return;
  }

  const partner = (stageGameType() === "cooperative") ? "TEAMMATE" : "OPPONENT";

  turnIndicatorEl.textContent = DRAW_IS_TAKE
    ? "TAKE A POD FOR YOURSELF"
    : `GIVE A POD TO ${partner}`;
}


function recordMove(blobId) {
    let blob = blobs.find(b => b.id === blobId);
    let letter = blob ? blob.letter : String.fromCharCode(65 + blobId);
    moves.push(letter);
    moveTimestamps.push(new Date().toLocaleString('en-US', { timeZone: 'America/New_York' }));
}

function playerSelectBlob(blobId) {
  selectionAllowed = false;
  let chosen = blobs.find(x => x.id === blobId);
  if (!chosen || chosen.selected) return;
  chosen.selected = true;

  const giveFirst = (!DRAW_IS_TAKE);

  // If TAKE: your first pick goes to you (left). If GIVE: your first pick goes to partner (right).
  const firstGoesToPlayer = !giveFirst;

  animateBlobToSide(chosen, firstGoesToPlayer, () => {
    if (firstGoesToPlayer) {
      playerScore += chosen.redCount;
      playerScoreEl.textContent = playerScore;
    } else {
      opponentScore += chosen.blueCount;
      opponentScoreEl.textContent = opponentScore;
    }
    updateScoreObj();

    // After the single human choice, all remaining pods are random draws.
    setTimeout(() => {
      playerTurn = false;
      updateTurnIndicator();

      if (firstGoesToPlayer) {
        opponentPick();     // TAKE: partner draws next
      } else {
        playerAutoPick();   // GIVE: you draw next (random)
      }
    }, HUMAN_TO_FIRST_AUTO_DELAY);
  });
}


function opponentPick(){
    let unselected = blobs.filter(b => !b.selected);
    if(unselected.length === 0){
    setTimeout(endRound, LAST_AUTO_TO_POPUP_DELAY);
    return;
    }
    let idx = Math.floor(Math.random() * unselected.length);
    let chosen = unselected[idx];
    recordMove(chosen.id);
    chosen.selected = true;
    animateBlobToSide(chosen, false, () => {
    opponentScore += chosen.blueCount;
    opponentScoreEl.textContent = opponentScore;
    updateScoreObj();
    let still = blobs.filter(b => !b.selected);
    if(still.length === 0){
        setTimeout(endRound, LAST_AUTO_TO_POPUP_DELAY);
    } else if(autoModeRound){
        schedulePlayerAuto();            // all remaining moves automated
    } else {
        playerTurn = true;
        selectionAllowed = true;
        updateTurnIndicator();
        displayBlobs();
    }
    });
}

function animateBlobToSide(blob, side, callback){
  const origEl = blob.element;
  if (!origEl) return;

  /* remove Select button (if present) */
  origEl.querySelector(".select-button-container")?.remove();

  /* ─── NO-ANIMATION branch ─────────────────────────────────────── */
  if (!ENABLE_ANIMATIONS){
    const destGrid = side ? selectedLeftEl : selectedRightEl;
    destGrid.appendChild(createStaticBlobContainer(blob));  // park it instantly
    origEl.remove();                                        // hide original
    callback?.();                                           // continue round logic
    return;
  }

  /* ─── ORIGINAL flying-clone logic (unchanged) ─────────────────── */
  const destGrid = side ? selectedLeftEl : selectedRightEl;
  const placeholder = createStaticBlobContainer(blob);
  placeholder.style.visibility = "hidden";
  destGrid.appendChild(placeholder);

  const root      = document.getElementById("gameRoot");
  const rootRect  = root.getBoundingClientRect();
  const startRect = origEl.getBoundingClientRect();
  const endRect   = placeholder.getBoundingClientRect();

  const startX = startRect.left - rootRect.left;
  const startY = startRect.top  - rootRect.top;
  const endX   = endRect.left   - rootRect.left;
  const endY   = endRect.top    - rootRect.top;

  const clone = origEl.cloneNode(true);
  clone.style.cssText = `
    position:absolute;
    left:${startX}px; top:${startY}px;
    width:${startRect.width}px; height:${startRect.height}px;
    transition:all 0.8s ease;
    z-index:9999; pointer-events:none;`;
  origEl.remove();
  root.appendChild(clone);

  requestAnimationFrame(() => {
    clone.style.left = endX + "px";
    clone.style.top  = endY + "px";
  });

  clone.addEventListener("transitionend", () => {
    clone.remove();
    placeholder.style.visibility = "";
    callback?.();
  }, { once:true });
}

function createStaticBlobContainer(blob){
    let cont = document.createElement("div");
    cont.classList.add("blob-container-static");
    cont.style.width = blobSize + "px";
    cont.style.height = blobSize + "px";
    let leftDiv = document.createElement("div");
    leftDiv.className = "blob-circles-left";
    leftDiv.style.width = "40%";
    for(let i = 0; i < blob.redCount; i++){
    let c = document.createElement("div");
    c.classList.add("circle", "red");
    leftDiv.appendChild(c);
    }
    cont.appendChild(leftDiv);
    let rightDiv = document.createElement("div");
    rightDiv.className = "blob-circles-right";
    rightDiv.style.width = "40%";
    for(let i = 0; i < blob.blueCount; i++){
    let c = document.createElement("div");
    c.classList.add("circle", "blue");
    rightDiv.appendChild(c);
    }
    cont.appendChild(rightDiv);
    return cont;
}

function endRound() {
  const stageGT = stageGameType();
  const base    = stageGT === "adversarial" ? BASE_PAYOFF_ADV : BASE_PAYOFF_COOP;
  const obj     = stageGT === "cooperative"
                    ? playerScore + opponentScore
                    : playerScore - opponentScore;
  const pay    = (base + VARIABLE_PAYOFF * obj).toFixed(2);
  const who     = stageGT === "cooperative" ? "Teammate" : "Opponent";
  const metric  = stageGT === "cooperative" ? "Score Sum" : "Score Difference";

  const roundLog = {
    subject_id: subjectID,
    round_number: currentRound,
    round_grp_number: Math.ceil(currentRound / NUM_CATEGORIES),
    n_blobs: ROUND_CONFIG[currentRound - 1].blobCount,
    draw_is_take: DRAW_IS_TAKE,
    round_start_time: roundStartTime,
    move1: moves[0] || null,
    timestamp1: moveTimestamps[0] || null,
    move2: moves[1] || null,
    timestamp2: moveTimestamps[1] || null,
    move3: moves[2] || null,
    timestamp3: moveTimestamps[2] || null,
    move4: moves[3] || null,
    timestamp4: moveTimestamps[3] || null,
    move5: moves[4] || null,
    timestamp5: moveTimestamps[4] || null,
    move6: moves[5] || null,
    timestamp6: moveTimestamps[5] || null,
    cum_score_you: playerScore,
    cum_score_opp: opponentScore,
    cum_score_obj: obj,
    round_payoff: pay,
    round_implemented: null,
    draw_mode: DRAW_IS_TAKE ? "take" : "give",
    balanced_cats: BALANCED_CATS,
  };
  roundLog.action_game_type = stageGameType();
  roundLog.action_condition = stagePods();

  // Populate CSV_COLUMNS if needed
  CSV_COLUMNS.forEach(col => {
    roundLog[col] = (ROUND_SAMPLES[currentRound - 1]?.[col] ?? null);
  });

  const line1 = DRAW_IS_TAKE
    ? `Your Score = ${playerScore}<br>`
    : `${who}'s Score = ${opponentScore}<br>`;

  const line2 = DRAW_IS_TAKE
    ? `${who}'s Score = ${opponentScore}<br>`
    : `Your Score = ${playerScore}<br>`;

  showModal(
    line1 +
    line2 +
    `${metric} = ${obj}<br>` +
    `If this round is selected for your payoff, you will earn $${pay}.`
  );

  // Detect “end of game” versus “end of first stage”
  const isOnlyOneStage = totalStages === 1;
  const isEndOfGame    = isOnlyOneStage
                        ? (stageRound === STAGES_ROUNDS[0])
                        : (currentStage === (totalStages - 1) && stageRound === STAGES_ROUNDS[currentStage]);

  if (isEndOfGame) {
    // Final‐round logic (mark one round implemented and show “Thanks for participating”)
    modalButtonEl.onclick = () => {
      closeModal();
    
      // 1) write the final round
      logRoundPayload(roundLog);

      finalChosenRound = Math.floor(Math.random() * totalRounds) + 1;
      try {
        window.parent.postMessage(
          { type: "ROUND_IMPLEMENTED", round_implemented: finalChosenRound },
          "*"
        );
      } catch(e){}

      fetch("/final_round_update", {
        method: "POST",
        headers: {"Content-Type":"application/json"},
        body: JSON.stringify({
          subject_id: subjectID,
          final_chosen_round: finalChosenRound
        })
      })
      .then(() => showFinal())
      .catch(console.error);
    };

  } else {
    // Multi‐stage logic: if it’s exactly end of the first stage, do transition
    const isEndOfFirstStage = (currentStage === 0 && stageRound === STAGES_ROUNDS[0]);
    if (isEndOfFirstStage) {
      modalButtonEl.onclick = () => {
        closeModal();
        logRoundPayload(roundLog);
        showStageTransition();
      };
    } else {
      // Normal round‐by‐round advance within a stage
      modalButtonEl.onclick = () => {
        closeModal();
        logRoundPayload(roundLog);

        if (stageRound < roundsPerStage) {
          nextRound();
        } else {
          // After finishing this stage, pick final round among all if it’s last stage:
          const Q = Math.floor(Math.random() * totalRounds) + 1;
          finalChosenRound = Q;
          showFinal();
        }
      };
    }
  }
}

function logRoundPayload(roundLog){

  fetch("/round", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(roundLog),
    cache: "no-store"
  }).catch(e => console.warn("round logging failed", e));

  gameLog.push(roundLog);
  try {
  window.parent.postMessage(
    { type:"GAME_LOG", subject_id: subjectID, gameLog: gameLog },
    "*"
  );
} catch(e){}
}


function resetQuizPlayButtonForStage() {
  const btn = document.getElementById("btnQuizPlay");
  if (!btn) return;
  btn.disabled = false;                 // allow once per stage
  btn.dataset.usedThisStage = "0";
}

function nextRound () {
  closeModal();
  currentRound++;   // keeps counting 1..12 for payoff lottery
  stageRound++;     // 1..6 on screen and in the log
  initRound();
}

function showStageTransition() {
  console.log("EXECUTING SIMPLIFIED TRANSITION");
  
  // Create transition message based on what's changing
  const [g1, g2] = STAGES_GAME_TYPES;     // game-type before / after
  const [b1, b2] = STAGES_BLOBS;          // pod-count before / after
  const isGame   = WITHIN_SUBJECT === "game_type";

  // Create message text with appropriate bold formatting
  const who1  = g1 === "adversarial" ? "<b>opponent</b>" : "<b>teammate</b>";
  const who2  = g2 === "adversarial" ? "<b>opponent</b>" : "<b>teammate</b>";
  const pods1 = `<b>${b1}</b>`;
  const pods2 = `<b>${b2}</b>`;

  const msg = isGame
    ? `Now you will play a different version of the game. `
      + `Instead of a computer ${who1}, you will now have a computer ${who2}. `
      + `The instructions and a demo will teach you how this new version of the game works.`
    : `Now you will play a different version of the game. `
      + `Instead of choosing from ${pods1} pods each round, you will now choose from ${pods2} pods per round. `
      + `The instructions and a demo will teach you how this new version of the game works.`;
  
  // Show modal with the transition message
  modalMessageEl.innerHTML = msg + "<br><br>";
  modalOverlayEl.style.display = "flex";
  modalButtonEl.style.display = "inline-block";
  modalButtonEl.disabled = true;
  
  // Critical: Make sure to enable the button after delay
  console.log("Setting button enable timeout");
  setTimeout(function() {
    console.log("Enabling transition button");
    modalButtonEl.disabled = false;
  }, 2000);
  
  // Set up the click handler for the transition button
  modalButtonEl.onclick = function () {
    closeModal();
  
    transitionDone = true;
    currentStage++;               // 0 → 1
    stageRound   = 1;
    currentRound = currentStage * roundsPerStage + 1;   // jump to 4 immediately
  
    resetDemoAndQuizElements();
    setPartnerLabels(currentStage);
    setupDemoPods();
    setupQuizPods();
    
    // Reset navigation buttons state
    const btnDemoPlay = document.getElementById("btnDemoPlay");
    const btnDemoContinue = document.getElementById("btnDemoContinue");
    if (btnDemoPlay) btnDemoPlay.disabled = false;
    if (btnDemoContinue) btnDemoContinue.disabled = true;
    
    // Reset quiz attempts
    quizAttempts = 0;
    firstTryCorrect = [null, null, null, null];
    
    // Update bottom text for both screens
    updateBottomText();
    
    // Update game screen instructions for when we return
    updateGameScreenInstructions();
    
    // Show the INSTRUCTIONS screen for stage 2 first in stage 2
    showScreen(1);

    console.log("TRANSITION COMPLETE - SHOWING INSTRUCTIONS FOR STAGE 2");

  };
}

// Add this new function to fully reset the demo and quiz elements
function resetDemoAndQuizElements() {
  // Clear demo containers
  document.getElementById("demoLeftGrid").innerHTML = "";
  document.getElementById("demoRightGrid").innerHTML = "";
  
  // Reset narration elements
  const demoNarr = document.getElementById("demoNarration");
  if (demoNarr) {
    demoNarr.innerHTML = "";
    demoNarr.style.display = "none";
  }
  
  // Clear quiz containers
  document.getElementById("quizLeftGrid").innerHTML = "";
  document.getElementById("quizRightGrid").innerHTML = "";
  document.getElementById("quizFormContainer").innerHTML = "";
  
  // Reset quiz narration
  const quizNarr = document.getElementById("quizNarration");
  if (quizNarr) {
    quizNarr.innerHTML = "";
    quizNarr.style.display = "none";
  }
  
  // Ensure the static pods elements are cleared for both demo and quiz
  document.getElementById("demoStaticPods").innerHTML = "";
  document.getElementById("quizStaticPods").innerHTML = "";

  updateBottomText();
}

function updateBottomText() {
  const gtThisStage = stageGameType();
  const partner = gtThisStage === "cooperative" ? "teammate" : "opponent";
  const metric = gtThisStage === "cooperative" 
    ? "Score Sum = Your Score + Teammate Score."
    : "Score Difference = Your Score – Opponent Score.";
  const basePayoff = gtThisStage === "adversarial" ? BASE_PAYOFF_ADV : BASE_PAYOFF_COOP;
  const payoffText = gtThisStage === "adversarial"
    ? `Your bonus = ($${VARIABLE_PAYOFF.toFixed(2)} × Score Difference) + $${basePayoff.toFixed(2)}.`
    : `Your bonus = $${VARIABLE_PAYOFF.toFixed(2)} × Score Sum.`;
  const payoffTextSum = gtThisStage === "adversarial"
    ? `YOUR RED DOTS minus OPPONENT'S BLUE DOTS determines BONUS.`
    : `YOUR RED DOTS plus TEAMMATE'S BLUE DOTS determines BONUS.`;

  const giveTakeHTML = DRAW_IS_TAKE
  ? `Every time you get a pod, you get points equal to its 
     <span style='color:red;font-weight:bold;'>red</span> dots, and your ${partner} gets nothing.<br>
     Every time your ${partner} gets a pod, they get points equal to its 
     <span style='color:blue;font-weight:bold;'>blue</span> dots, and you get nothing.<br>`
  : `Every time your ${partner} gets a pod, they get points equal to its 
     <span style='color:blue;font-weight:bold;'>blue</span> dots, and you get nothing.<br>
     Every time you get a pod, you get points equal to its 
     <span style='color:red;font-weight:bold;'>red</span> dots, and your ${partner} gets nothing.<br>`;

  const bottomHTML = `
    ${giveTakeHTML}
    At the end of each round, new pods will appear.<br><br>
    <span style='font-weight:bold;'>${metric}<br>${payoffText}</span>
  `;

  // NEW: instructions bottom text
  const instrBottomText = document.getElementById('instrBottomText');
  if (instrBottomText) {
    instrBottomText.innerHTML = bottomHTML;
  }

  // Demo bottom text
  const demoBottomText = document.getElementById('demoBottomText');
  if (demoBottomText) {
    demoBottomText.innerHTML = bottomHTML;
  }

  // Quiz bottom text
  const quizBottomText = document.getElementById('quizBottomText');
  if (quizBottomText) {
    quizBottomText.innerHTML = bottomHTML;
  }
}

function showFinal () {
  try {
  window.parent.postMessage(
    { type:"GAME_LOG", subject_id: subjectID, gameLog: gameLog },
    "*"
  );
} catch(e){}
  /* ①  cache the final screen so reloads land here */
  const finalHTML = `
    <div style="width:100%;height:100vh;display:flex;justify-content:center;align-items:center;text-align:center;padding:0 20px;">
      <h2 style="font-size:2em;line-height:1.3;">
        Scroll down and enter the code <strong>1234</strong> to continue the survey.
      </h2>
    </div>`;

  /* ②  stamp “end” in the DB */
  fetch("/subject_update", {
    method : "POST",
    headers: { "Content-Type":"application/json" },
    body   : JSON.stringify({ subject_id: subjectID, stage: "end" })
  });

  /* ③  swap the whole game UI for the final message */
  closeModal();
  document.getElementById("gameRoot").innerHTML = finalHTML;
}

function showModal(msg, showBtn=true){
    modalMessageEl.innerHTML = msg + "<br><br>";
    modalOverlayEl.style.display = "flex";
    if (showBtn) {
      modalButtonEl.style.display = "inline-block";
      modalButtonEl.disabled = true;
      setTimeout(()=>{ modalButtonEl.disabled = false; }, 1000);
    }
}

function closeModal(){
    modalOverlayEl.style.display = "none";
}

document.addEventListener("DOMContentLoaded", () => {
    // 1️⃣ Initial layout + labels
    setupDemoPods();
    setPartnerLabels();
    updateBottomText();
  
    // 2️⃣ Demo page buttons
    const btnDemoPlay     = document.getElementById("btnDemoPlay");
    const btnDemoContinue = document.getElementById("btnDemoContinue");
    btnDemoPlay.onclick = () => {
      const route = currentStage === 0 ? "/subject_update" : "/stage_update";
      const stage = currentStage === 0 ? "demo"          : "demo2";
      fetch(route, {
        method: "POST",
        headers: {"Content-Type":"application/json"},
        body: JSON.stringify({subject_id: subjectID, stage})
      });
      btnDemoPlay.disabled     = true;
      btnDemoContinue.disabled = true;
      playDemo();
    };
    btnDemoContinue.disabled = true;  // will be enabled in showNarration()
  
    // 3️⃣ Quiz page buttons
    const btnQuizPlay = document.getElementById("btnQuizPlay");
btnQuizPlay.onclick = () => {
  if (btnQuizPlay.dataset.usedThisStage === "1") return; // extra safety
  btnQuizPlay.dataset.usedThisStage = "1";
  btnQuizPlay.disabled = true;

  setupQuizPods();
  playQuizDemo();
};

  
    // 4️⃣ Screen‑transition: Demo ➔ Quiz
    document.getElementById("btnDemoContinue").onclick = () => {
      showScreen(3);
      setPartnerLabels();
    };
  });

function restartGame(){
    window.location.reload();
}