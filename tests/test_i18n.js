const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const dictionary = JSON.parse(fs.readFileSync('www/i18n-zh.json', 'utf8'));
let lang = 'zh', callback, observes = 0;
const node = text => ({nodeValue: text, parentElement: {closest: () => null}});
const text = node('  Extraction\n Method  ');
const ambiguous = node('Disease Nature (病性)');
const raw = node('Extraction Method');
raw.parentElement.closest = selector => selector.includes('tbody') ? {} : null;
const option = {value: 'Ward (hierarchical)', hasAttribute: () => false,
  setAttribute: (key, value) => { option.savedValue = value; }};
const choice = node('Ward (hierarchical)');
choice.parentElement.closest = selector => selector === 'option' ? option : null;
const nodes = [text, ambiguous, raw, choice];
const context = {
  window: {coreTCMTranslations: dictionary}, NodeFilter: {SHOW_TEXT: 4},
  document: {body: {querySelectorAll: () => []}, documentElement: {},
    getElementById: () => ({value: lang}),
    createTreeWalker: () => {let i=0; return {nextNode: () => nodes[i++]};}},
  MutationObserver: class {constructor(fn) {callback=fn;} disconnect() {} observe() {observes++;}},
  setTimeout: fn => fn(),
  $: arg => {if (typeof arg === 'function') arg(); return {on() {return this;}};}
};
vm.runInNewContext(fs.readFileSync('www/i18n.js', 'utf8'), context);
assert.equal(text.nodeValue, '  提取方法  ');
assert.equal(raw.nodeValue, 'Extraction Method');
assert.equal(option.savedValue, 'Ward (hierarchical)');
assert.equal(choice.nodeValue, 'Ward 层次聚类');
for (let i=0;i<3;i++) {
  lang='en'; callback();
  assert.equal(text.nodeValue, '  Extraction\n Method  ');
  assert.equal(ambiguous.nodeValue, 'Disease Nature (病性)');
  lang='zh'; callback();
  assert.equal(ambiguous.nodeValue, '病性');
}
text.nodeValue='Train the model first.'; callback();
assert.equal(text.nodeValue,'请先训练模型。');
lang='en'; callback();
assert.equal(text.nodeValue,'Train the model first.');
assert.equal(observes,9);
console.log('Client translation: whitespace, round trips, updated nodes, raw data and option values passed');
