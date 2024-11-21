const fs = require('fs');

const nifty = require('nifty');
const util = require('./util.js');

const getStandardItems = function() {
  const items = [];

  // ** Applications ** //

  items.push(nifty.lib.mkSimple({
    text: 'Firefox',
    exec: () => util.exec('firefox'),
    icon: util.mkIcon('./icons/firefox.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Firefox (alt profile)',
    exec: () => util.exec('firefox-alt'),
    icon: util.mkIcon('./icons/firefox.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Discord',
    exec: () => util.exec('discord'),
    icon: util.mkIcon('./icons/discord.svg'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Telegram',
    exec: () => util.exec('telegram-desktop'),
    icon: util.mkIcon('./icons/telegram.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'KeePassXC',
    exec: () => util.exec('keepassxc'),
    icon: util.mkIcon('./icons/keepass.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Chrome',
    exec: () => util.exec('google-chrome-stable'),
    icon: util.mkIcon('./icons/chrome.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Chrome (transient)',
    exec: () => util.exec('google-chrome-stable --user-data-dir=chrome-temp-profile'),
    icon: util.mkIcon('./icons/chrome.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Chrome (transient + insecure)',
    exec: () => util.exec('google-chrome-stable --user-data-dir=chrome-temp-profile --disable-web-security'),
    icon: util.mkIcon('./icons/chrome.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Peek',
    exec: () => util.exec('peek'),
    icon: util.mkIcon('./icons/screenshot.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'OBS Studio',
    exec: () => util.exec('obs'),
    icon: util.mkIcon('./icons/obs.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Steam',
    exec: () => util.exec('steam'),
    icon: util.mkIcon('./icons/steam.png'),
  }));

  // ** Commands ** //

  items.push(nifty.lib.mkSimple({
    text: 'Machine: Screen off',
    exec: () => util.exec('xset dpms force off'),
    icon: util.mkIcon('./icons/screen-sleep.svg'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Machine: Suspend',
    exec: () => util.exec('sudo pm-suspend'),
    icon: util.mkIcon('./icons/screen-sleep.svg'),
  }));

  // Screenshotting

  items.push(nifty.lib.mkSimple({
    text: 'Screenshot',
    exec: () => util.exec(`flameshot gui -r | xclip -selection clipboard -t image/png`),
      // From <https://github.com/flameshot-org/flameshot/issues/635#issuecomment-2302675095>
      // There may be a better solution available though? Something abut a daemon?
    icon: util.mkIcon('./icons/screenshot.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Screenshot (fullscreen)',
    exec: () => util.exec(`flameshot gui -r --region all | xclip -selection clipboard -t image/png`),
      // <https://github.com/flameshot-org/flameshot/issues/635#issuecomment-2302675095>
    icon: util.mkIcon('./icons/screenshot.png'),
  }));


  // Copy to cliboard

  items.push(nifty.lib.mkSimple({
    text: 'Copy: ZWS (zero-width space)',
    exec: () => util.copyToClipboard(String.fromCharCode(8203)),
    icon: util.mkIcon('./icons/copy-to-clipboard.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Copy: NBSP (non-breaking space)',
    exec: () => util.copyToClipboard(String.fromCharCode(160)),
    icon: util.mkIcon('./icons/copy-to-clipboard.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Copy: ¢ (cents symbol)',
    exec: () => util.copyToClipboard('¢'),
    icon: util.mkIcon('./icons/copy-to-clipboard.png'),
  }));


  // Dark/Light mode

  function setWeztermColorScheme(name) {
    fs.writeFileSync(`${process.env.HOME}/.config/wezterm/current-color-scheme`, name);
  }

  function setKakouneColorScheme(name) {
    util.exec(String.raw`
      target="$HOME/.config/kak/current-color-scheme.kak"
      mkdir -p "$(dirname "$target")" && \
      echo 'colorscheme ${name}' > "$target" && \
      kak -l | while read session_id; do echo "source $target" | kak -p $session_id; done
    `);
  }

  items.push(nifty.lib.mkSimple({
    text: 'Mode: Dark',
    exec: () => {
      setWeztermColorScheme('Wombat');
      setKakouneColorScheme('tomorrow-night');
    },
    icon: util.mkIcon('./icons/dark-mode-light-mode.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Mode: Light',
    exec: () => {
      setWeztermColorScheme('Papercolor Light (Gogh)');
      setKakouneColorScheme('kaleidoscope-light');
    },
    icon: util.mkIcon('./icons/dark-mode-light-mode.png'),
  }));


  // Screen layouts

  function setAlacrittyFontSize(size) {
    util.exec(`echo 'font: { size: ${size} }' > ~/.config/alacritty/current-font-size.yml`);
  }

  items.push(nifty.lib.mkSimple({
    text: 'Layout: One',
    exec: () => {
      util.exec('xrandr --output eDP-1 --auto --mode 1920x1200 --output HDMI-1 --off');
      setAlacrittyFontSize(6);
    },
    icon: util.mkIcon('./icons/screen-layout.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Layout: Other',
    exec: () => {
      util.exec('xrandr --output HDMI-1 --auto --output eDP-1 --off');
      setAlacrittyFontSize(9);
    },
    icon: util.mkIcon('./icons/screen-layout.png'),
  }));

  /*

  items.push(nifty.lib.mkSimple({
    text: 'Layout: Both',
    exec: () => util.exec('xrandr --output eDP-1 --auto --mode 1920x1200 --output HDMI-1 --auto --above eDP-1'),
    icon: util.mkIcon('./icons/screen-layout.png'),
  }));

  items.push(nifty.lib.mkSimple({
    text: 'Layout: Clone',
    exec: () => util.exec('xrandr --output HDMI-1 --auto --output eDP-1 --same-as HDMI-1'),
    icon: util.mkIcon('./icons/screen-layout.png'),
  }));

  */

  return items;
};


nifty.run(query => {

  query = query.trim();

  // Emoji input
  // Use 'em' for emoji, 'ems' for small emoji (when pasted into Telegram)
  if (query.startsWith('em ') || query.startsWith('ems ')) {
    const prefix = query.split(' ')[0];
    const doSmall = prefix === 'ems';

    const emojiInfo = JSON.parse(fs.readFileSync(plib.resolve(__dirname, './emoji/emoji-info.json')));
    const wideSpace = ' ';
    const zws = String.fromCharCode(8203);
    let items = (
      Object.entries(emojiInfo)
      .sort((e1, e2) => e1[0].localeCompare(e2[0]))
      .map(([emoji, keywords]) => nifty.lib.mkItem({
        searchText: emoji + ' ' + keywords.join(', '),
        displayText: emoji + wideSpace + keywords[0],
        exec: () => util.copyToClipboard(doSmall ? emoji + zws : emoji),
        isSticky: true,
      }))
    );

    items = nifty.lib.sort(items, query.slice(prefix.length + ' '.length));
    items = items.slice(0, 8);
    return items;
  }

  // LaTeX input
  //
  // Example: '\mathcal{AB}'
  // Will ignore leading "\ ";
  // Example: '\ if \mathcal{A} \subseteq ...'
  if (query.includes('\\')) {
    const latex = (
      query.startsWith('\\ ')
      ? query.slice(2)
      : query
    );
    const unicode = util.exec(['latuc', latex], { capture: true }).trim();
    return [clipboard(unicode)];
  }

  // Brightness & volume control
  if (query.startsWith('bri ') || query.startsWith('vol ')) {
    const isBri = query.startsWith('bri');

    let value = +query.slice('bri '.length);
    if (!Number.isFinite(value)) value = 50;
    value = Math.max(value, 0);
    value = Math.min(value, 100);

    const nbsp = String.fromCharCode(160);
    return [nifty.lib.mkSimple({
      text: (
        (isBri ? '☀️' : '🔊') + repeat(nbsp, 5)
        + mkBar(value / 100, 35)
      ),
      exec() {
        if (isBri) {
          util.exec(`light -S ${value}`);
        } else {
          util.exec(`pactl set-sink-volume @DEFAULT_SINK@ ${value}%`);
        }
        util.exec('pkill --signal SIGUSR2 xmobar');  // refresh status bar
      },
    })];
  }

  if (query === '/shrug')
    return [clipboard('¯\\_(ツ)_/¯')];

  if (query === '')
    return [];

  if (query === '*')
    return getStandardItems();


  // Otherwise ...

  let items = getStandardItems();
  items = nifty.lib.sort(items, query);
  items = items.slice(0, 4);

  return items;

});

function clipboard(text) {
  return nifty.lib.mkSimple({
    text,
    exec: () => util.copyToClipboard(text),
    isSticky: true
  });
}


// Ported from elsewhere
function mkBar(progress, width) {
  const n = progress * width;
  const initial_n = Number.isInteger(n) && n > 0 ? n - 1 : Math.floor(n);
  const partial_v = progress * width - initial_n;
  const empty_n = width - (initial_n + 1);

  return (
    [ repeat("━", initial_n)
    , partial_v <= 0/8 ? "─"
      : partial_v <= 4/8 ? "╾"
      : partial_v <= 8/8 ? "━"
      : null
    , repeat("─", empty_n)
    ].join("")
  );
}

function repeat(s, n) {
  let r = '';
  while (n--) r += s;
  return r;
}

