from pathlib import Path
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.patches import Circle, Rectangle
from matplotlib import font_manager as fm

PREFERRED_FONTS = ["Latin Modern Roman", "LM Roman 10", "FreeSerif", "Nimbus Roman", "STIXGeneral", "DejaVu Serif"]
available = {f.name for f in fm.fontManager.ttflist}
FONT_NAME = next((n for n in PREFERRED_FONTS if n in available), "DejaVu Serif")

matplotlib.rcParams["font.family"] = FONT_NAME
matplotlib.rcParams["mathtext.fontset"] = "stix"
matplotlib.rcParams["figure.dpi"] = 300
matplotlib.rcParams["savefig.dpi"] = 300

RED, BLUE, GRAY_BG = "#ff2020", "#2b40ff", "#ececec"

# Layout uses x in 0..1, stretched to XMAX so circles stay round
FIG_W, FIG_H = 20, 9
XMAX = FIG_W / FIG_H

fig = plt.figure(figsize=(FIG_W, FIG_H))
ax = plt.axes([0, 0, 1, 1])
ax.set_xlim(0, XMAX)
ax.set_ylim(0, 1.0)
ax.set_aspect("equal", adjustable="box")
ax.axis("off")


def sx(x):
    return x * XMAX


def centered_text(x, y, text, fontsize=18, weight="normal"):
    ax.text(sx(x), y, text, ha="center", va="top", fontsize=fontsize, fontweight=weight, color="black", family=FONT_NAME)


def centered_segments(x, y, segments, fontsize=18):
    """One centered line of differently styled text segments: (text, weight, color)."""
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    widths = []
    for text, weight, color in segments:
        t = ax.text(0, 0, text, fontsize=fontsize, fontweight=weight, color=color, family=FONT_NAME, alpha=0, transform=ax.transData)
        widths.append(t.get_window_extent(renderer=renderer).width)
        t.remove()

    x_disp, y_disp = ax.transData.transform((sx(x), y))
    cur = x_disp - sum(widths) / 2
    for (text, weight, color), w in zip(segments, widths):
        x_data, _ = ax.transData.inverted().transform((cur, y_disp))
        ax.text(x_data, y, text, ha="left", va="top", fontsize=fontsize, fontweight=weight, color=color,
                family=FONT_NAME, transform=ax.transData)
        cur += w


def draw_pod(cx, cy, r, n_red, n_blue, panel_h):
    ax.add_patch(Circle((cx, cy), r, facecolor="white", edgecolor="black", linewidth=1.9))
    dot_r, row_gap, col_gap, top_y = r * 0.105, r * 0.300, r * 0.345, cy + r * 0.60
    for x0, n, color in [(cx - r * 0.58, n_red, RED), (cx + r * 0.34, n_blue, BLUE)]:
        for k in range(n):
            ax.add_patch(Circle((x0 + (k % 2) * col_gap, top_y - (k // 2) * row_gap), dot_r, facecolor=color, edgecolor="none"))

    btn_w, btn_h = r * 1.02, panel_h * 0.062
    by = cy - r + panel_h * 0.028
    ax.add_patch(Rectangle((cx - btn_w / 2, by), btn_w, btn_h, facecolor="#f6f6f6", edgecolor="#bdbdbd", linewidth=0.9))
    ax.text(cx, by + btn_h / 2, "Select", ha="center", va="center", fontsize=12.0, family=FONT_NAME)


def draw_panel(x, y, w, h, label, counts):
    top_red, top_blue, bot_red, bot_blue = counts
    ax.text(sx(x) - 0.008 * XMAX, y + h + 0.006, label, ha="center", va="bottom", fontsize=22, fontweight="bold", family=FONT_NAME)
    ax.add_patch(Rectangle((sx(x), y), w * XMAX, h, facecolor=GRAY_BG, edgecolor="none"))
    cx = sx(x + w / 2)
    r = min(w * XMAX * 0.295, h * 0.182)
    draw_pod(cx, y + h * 0.748, r, top_red, top_blue, h)
    draw_pod(cx, y + h * 0.282, r, bot_red, bot_blue, h)


def draw_side(x0, x1, side_label, partner, formula1, formula2, panel_counts):
    cx = (x0 + x1) / 2
    ax.text(sx(x0 + 0.02), 0.975, side_label, ha="left", va="top", fontsize=27, fontweight="bold", family=FONT_NAME)

    y, gap = 0.935, 0.039
    centered_text(cx, y, "In each round, you will choose the first pod and take it for yourself.")
    centered_text(cx, y - gap, f"Then your {partner} will get the remaining pod.")
    centered_segments(cx, y - 2 * gap, [
        ("Every time you get a pod, you get points equal to its ", "normal", "black"),
        ("red", "bold", RED),
        (f" dots, and your {partner} gets nothing.", "normal", "black"),
    ])
    centered_segments(cx, y - 3 * gap, [
        (f"Every time your {partner} gets a pod, they get points equal to its ", "normal", "black"),
        ("blue", "bold", BLUE),
        (" dots, and you get nothing.", "normal", "black"),
    ])
    centered_text(cx, 0.775, formula1, fontsize=18.2, weight="bold")
    centered_text(cx, 0.738, formula2, fontsize=18.2, weight="bold")

    pad, panel_gap, panel_h, panel_y = 0.043, 0.032, 0.555, 0.070
    panel_w = (x1 - x0 - 2 * pad - panel_gap) / 2
    for px, label, counts in zip([x0 + pad, x0 + pad + panel_w + panel_gap], ["i.", "ii."], panel_counts):
        draw_panel(px, panel_y, panel_w, panel_h, label, counts)


ax.plot([sx(0.5), sx(0.5)], [0.02, 0.965], color="#9a9a9a", linewidth=1.0)

draw_side(0.00, 0.50, "a.", "opponent",
          "Score Difference = Your Score − Opponent Score.",
          "Your bonus = (\\$0.10 × Score Difference) + \\$1.00.",
          [(4, 6, 7, 5), (4, 6, 7, 1)])
draw_side(0.50, 1.00, "b.", "teammate",
          "Score Sum = Your Score + Teammate Score.",
          "Your bonus = $0.10 × Score Sum.",
          [(5, 2, 7, 3), (5, 2, 7, 5)])

out_png = Path("tables_and_charts/main/fig1_pods.png")
fig.savefig(out_png, bbox_inches="tight", pad_inches=0, facecolor="white")
plt.close(fig)
