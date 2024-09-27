
// Bruh
Set.prototype.union = function(other) {
  const result = new Set();
  for (const a of this) result.add(a);
  for (const b of other.keys()) result.add(b);
  return result;
}
Set.prototype.difference = function(other) {
  const result = new Set(this);
  for (const x of other.keys()) result.delete(x);
  return result;
}


main();

async function main() {

  // { emoji -> { name : string, terms : set string } }
  const emojis = {};

  function append(emoji, name, terms) {
    if (!(emoji in emojis)) {
      emojis[emoji] = { name: null, terms: new Set() };
    }
    emojis[emoji].name ||= name;
    emojis[emoji].terms = (
      emojis[emoji].terms
      .union(terms)
      .union(new Set([name]))
    );
  }

  // Source 1
  {
    const src = `https://raw.githubusercontent.com/Mange/rofi-emoji/7e2f502ed15f25a035dceb84be9c980741ba9179/all_emojis.txt`;
    console.log(src);
    const data = await (await fetch(src)).text();
    for (const line of data.split('\n')) {
      if (!line.trim()) continue;
      const [emoji, _group, _subgroup, name, termsStr] = line.split('\t');
      const terms = new Set(termsStr.split(' | ').map(term => term.trim()).filter(term => !!term));
      append(emoji, name, terms);
    }
  }

  // -- Transform -- //

  // Special modificatoins
 emojis['🤨'].terms.add('suspicious'); 

  // Filter
  const toRemove = new Set();
  for (const [emoji, info] of Object.entries(emojis)) {

    // keep only yellow skin-tone emoji
    if ([...info.terms].some(term => term.includes('skin tone')))
      toRemove.add(emoji);

    // exclude emoji with no search terms
    if (info.terms.size === 0)
      toRemove.add(emoji);

    // exclude flags
    if (info.terms.has('flag'))
      toRemove.add(emoji);

    // exclude text versions of emoji
    const emojiVariantSelector = String.fromCharCode(65039);
    if ((emoji + emojiVariantSelector) in emojis)
      toRemove.add(emoji);

    // exclude text versions of keycap
    const keycapCombinator = String.fromCharCode(8419);
    if (
      emoji.endsWith(keycapCombinator)
      && [...emoji].length === 2
      && (Array.from(emoji)[0] + emojiVariantSelector + keycapCombinator) in emojis
    )
      toRemove.add(emoji);

  }
  for (const emoji of toRemove) {
    delete emojis[emoji];
  }

  // Emit
  const result = {};
  for (const [emoji, info] of Object.entries(emojis)) {
    result[emoji] = [].concat([info.name], Array.from(info.terms.difference(new Set([info.name]))));
  }
  require('fs').writeFileSync('./emoji-info.json', JSON.stringify(result, null, 2));

}

