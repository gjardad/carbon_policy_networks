import { PDFDocument } from 'pdf-lib';
import fs from 'fs/promises';
import path from 'path';

const inputPath = 'articles/de_lyon_dechezlepretre_2025.pdf';
const outputDir = 'articles/split_de_lyon_dechezlepretre_2025';
const pagesPerChunk = 4;
const prefix = 'de_lyon_dechezlepretre_2025';

await fs.mkdir(outputDir, { recursive: true });
const bytes = await fs.readFile(inputPath);
const src = await PDFDocument.load(bytes);
const total = src.getPageCount();

let chunks = 0;
for (let start = 0; start < total; start += pagesPerChunk) {
  const end = Math.min(start + pagesPerChunk, total);
  const out = await PDFDocument.create();
  const indices = [];
  for (let i = start; i < end; i++) indices.push(i);
  const copied = await out.copyPages(src, indices);
  copied.forEach(p => out.addPage(p));
  const outBytes = await out.save();
  const name = `${prefix}_pp${start + 1}-${end}.pdf`;
  await fs.writeFile(path.join(outputDir, name), outBytes);
  chunks++;
}

console.log(`Split ${total} pages into ${chunks} chunks in ${outputDir}`);
