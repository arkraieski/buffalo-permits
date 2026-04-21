import fs from "node:fs/promises";
import path from "node:path";
import puppeteer from "puppeteer";

const rootDir = process.cwd();
const siteDir = path.resolve(rootDir, process.argv[2] ?? "_site");
const snapshotPath = path.join(siteDir, "og-snapshot.json");
const outputPath = path.join(siteDir, "opengraph.png");

function textOrEmpty(value) {
  return typeof value === "string" ? value.trim() : "";
}

function escapeHtml(value) {
  return value
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#39;");
}

async function fileToDataUrl(filePath) {
  const extension = path.extname(filePath).toLowerCase();
  const mimeType = extension === ".png" ? "image/png" : "image/jpeg";
  const bytes = await fs.readFile(filePath);
  return `data:${mimeType};base64,${bytes.toString("base64")}`;
}

async function readSiteSnapshot() {
  const snapshotRaw = await fs.readFile(snapshotPath, "utf8");
  return JSON.parse(snapshotRaw);
}

function buildMarkup(snapshot) {
  const metricMarkup = snapshot.kpis
    .map(
      (item) => `
        <div class="metric-card">
          <div class="metric-value">${escapeHtml(item.value)}</div>
          <div class="metric-label">${escapeHtml(item.label)}</div>
        </div>
      `
    )
    .join("");

  return `<!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="utf-8" />
      <style>
        :root {
          --bg: #f6efe7;
          --ink: #13232f;
          --muted: #55646f;
          --accent: #d1495b;
          --accent-soft: rgba(209, 73, 91, 0.14);
          --card: rgba(255, 250, 246, 0.9);
          --line: rgba(19, 35, 47, 0.12);
        }

        * {
          box-sizing: border-box;
        }

        body {
          margin: 0;
          width: 1200px;
          height: 630px;
          font-family: "Avenir Next", "Segoe UI", Arial, sans-serif;
          color: var(--ink);
          background:
            radial-gradient(circle at top left, rgba(209, 73, 91, 0.18), transparent 36%),
            linear-gradient(135deg, #fff8f2 0%, #f6efe7 48%, #efe2d4 100%);
        }

        .frame {
          position: relative;
          display: grid;
          grid-template-columns: 1.08fr 0.92fr;
          width: 100%;
          height: 100%;
          overflow: hidden;
        }

        .frame::after {
          content: "";
          position: absolute;
          inset: 0;
          border: 1px solid rgba(19, 35, 47, 0.08);
          pointer-events: none;
        }

        .content {
          padding: 52px 48px 42px 56px;
          display: flex;
          flex-direction: column;
          justify-content: space-between;
          gap: 24px;
        }

        .eyebrow {
          display: inline-flex;
          align-items: center;
          gap: 10px;
          font-size: 16px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: var(--accent);
        }

        .eyebrow::before {
          content: "";
          width: 34px;
          height: 3px;
          border-radius: 999px;
          background: var(--accent);
        }

        h1 {
          margin: 0;
          max-width: 580px;
          font-size: 60px;
          line-height: 1.02;
          letter-spacing: -0.04em;
        }

        .summary {
          display: flex;
          flex-direction: column;
          gap: 14px;
          max-width: 560px;
          font-size: 23px;
          line-height: 1.35;
          color: var(--muted);
        }

        .meta-row {
          display: flex;
          flex-wrap: wrap;
          gap: 12px;
        }

        .meta-pill {
          padding: 14px 18px;
          min-width: 210px;
          border-radius: 18px;
          border: 1px solid var(--line);
          background: var(--card);
          backdrop-filter: blur(8px);
        }

        .meta-label {
          margin-bottom: 6px;
          font-size: 14px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: var(--accent);
        }

        .meta-value {
          font-size: 22px;
          line-height: 1.2;
          color: var(--ink);
        }

        .metrics {
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 12px;
        }

        .metric-card {
          padding: 18px 16px 16px;
          border-radius: 20px;
          background: rgba(19, 35, 47, 0.94);
          color: white;
          box-shadow: 0 18px 35px rgba(19, 35, 47, 0.16);
          min-width: 0;
        }

        .metric-value {
          font-size: 25px;
          line-height: 1.02;
          font-weight: 800;
          letter-spacing: -0.04em;
          overflow-wrap: anywhere;
        }

        .metric-label {
          margin-top: 10px;
          font-size: 15px;
          line-height: 1.25;
          color: rgba(255, 255, 255, 0.72);
          overflow-wrap: anywhere;
        }

        .site-name {
          font-size: 18px;
          font-weight: 700;
          color: var(--ink);
          opacity: 0.78;
        }

        .visual {
          position: relative;
          overflow: hidden;
          background: #0d1820;
        }

        .visual img {
          width: 100%;
          height: 100%;
          object-fit: cover;
          object-position: center;
          filter: saturate(1.05) contrast(1.03);
          transform: scale(1.03);
        }

        .visual::before {
          content: "";
          position: absolute;
          inset: 0;
          background:
            linear-gradient(180deg, rgba(7, 16, 22, 0.08), rgba(7, 16, 22, 0.44)),
            linear-gradient(135deg, rgba(19, 35, 47, 0.04), rgba(209, 73, 91, 0.2));
        }

        .visual::after {
          content: "";
          position: absolute;
          left: -80px;
          top: 36px;
          width: 180px;
          height: 180px;
          border-radius: 50%;
          background: rgba(255, 248, 242, 0.12);
          border: 1px solid rgba(255, 248, 242, 0.18);
        }
      </style>
    </head>
    <body>
      <main class="frame">
        <section class="content">
          <div>
            <div class="eyebrow">Buffalo dashboard</div>
            <h1>${escapeHtml(snapshot.title)}</h1>
          </div>

          <div class="summary">
            <div class="meta-row">
              <div class="meta-pill">
                <div class="meta-label">Window</div>
                <div class="meta-value">${escapeHtml(snapshot.windowLabel)}</div>
              </div>
              <div class="meta-pill">
                <div class="meta-label">Updated</div>
                <div class="meta-value">${escapeHtml(snapshot.updatedLabel)}</div>
              </div>
            </div>
          </div>

          <div class="metrics">${metricMarkup}</div>

          <div class="site-name">${escapeHtml(snapshot.pageTitle)}</div>
        </section>

        <aside class="visual">
          <img src="${snapshot.heroDataUrl}" alt="" />
        </aside>
      </main>
    </body>
  </html>`;
}

async function main() {
  const browser = await puppeteer.launch({
    headless: true,
    args: ["--no-sandbox", "--disable-setuid-sandbox"]
  });

  try {
    const snapshot = await readSiteSnapshot();
    const heroPath = path.resolve(siteDir, textOrEmpty(snapshot.heroSrc));
    snapshot.heroDataUrl = await fileToDataUrl(heroPath);
    snapshot.description =
      snapshot.description ||
      "Daily Buffalo dashboard covering permits, crime incidents, and demolition activity.";

    const page = await browser.newPage();
    await page.setViewport({ width: 1200, height: 630, deviceScaleFactor: 1 });
    await page.setContent(buildMarkup(snapshot), { waitUntil: "load" });
    await page.screenshot({ path: outputPath, type: "png" });
    await page.close();

    console.log(`Generated ${outputPath}`);
  } finally {
    await browser.close();
  }
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
