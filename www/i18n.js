(function () {
  'use strict';
  const normalize = s => s.replace(/\s+/g, ' ').trim();
  const dictionary = Object.create(null);
  Object.entries(window.coreTCMTranslations || {}).forEach(([key, value]) => {
    dictionary[normalize(key)] = value;
  });
  // Remember each original node, rather than reversing a many-to-one dictionary.
  const originals = new WeakMap();
  const attributes = new WeakMap();
  const skipped = 'script,style,textarea,code,[contenteditable="true"],[data-i18n="off"],.dataTable tbody';
  function translate(source, chinese) {
    if (!chinese) return source;
    const translated = dictionary[normalize(source)];
    if (translated === undefined) {
      return source.replace(/^Showing ([\d,]+) to ([\d,]+) of ([\d,]+) entries$/, '显示第 $1 至 $2 条，共 $3 条记录')
        .replace(/^\(filtered from ([\d,]+) total entries\)$/, '（从 $1 条记录中筛选）');
    }
    return source.match(/^\s*/)[0] + translated + source.match(/\s*$/)[0];
  }
  function update(owner, property, cache, chinese) {
    const current = owner[property];
    let record = cache.get(owner);
    if (!record || current !== record.rendered) record = {source: current};
    const rendered = translate(record.source, chinese);
    if (rendered !== current) owner[property] = rendered;
    record.rendered = rendered;
    cache.set(owner, record);
  }
  let observer;
  function apply() {
    if (!document.body) return;
    observer.disconnect();
    const chinese = document.getElementById('app_lang')?.value !== 'en';
    document.documentElement.lang = chinese ? 'zh-CN' : 'en';
    const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT);
    let node;
    while ((node = walker.nextNode())) {
      if (!node.parentElement || node.parentElement.closest(skipped)) continue;
      // Explicit values prevent translated option labels changing submitted values.
      const option = node.parentElement.closest('option');
      if (option && !option.hasAttribute('value')) option.setAttribute('value', option.value);
      update(node, 'nodeValue', originals, chinese);
    }
    document.body.querySelectorAll('[title],[placeholder],[aria-label]').forEach(element => {
      if (element.closest(skipped)) return;
      let cache = attributes.get(element);
      if (!cache) { cache = new Map(); attributes.set(element, cache); }
      ['title', 'placeholder', 'aria-label'].forEach(name => {
        if (!element.hasAttribute(name)) return;
        const current = element.getAttribute(name);
        let record = cache.get(name);
        if (!record || current !== record.rendered) record = {source: current};
        record.rendered = translate(record.source, chinese);
        if (current !== record.rendered) element.setAttribute(name, record.rendered);
        cache.set(name, record);
      });
    });
    observer.observe(document.body, {childList: true, subtree: true, characterData: true,
      attributes: true, attributeFilter: ['title', 'placeholder', 'aria-label']});
  }
  let pending = false;
  function schedule() {
    if (pending) return;
    pending = true;
    setTimeout(() => { pending = false; apply(); }, 0);
  }
  observer = new MutationObserver(schedule);
  $(document).on('change', '#app_lang', schedule);
  $(document).on('shiny:value shiny:connected shiny:bound', schedule);
  $(schedule);
})();
