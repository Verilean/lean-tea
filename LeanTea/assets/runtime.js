(function(){
const synth = window.speechSynthesis
let voices = []
function loadVoices(){ voices = synth.getVoices() }
if (synth.onvoiceschanged !== undefined) synth.onvoiceschanged = loadVoices
loadVoices()
function pickVoice(){
  const prefs = [
    v => /en[-_]US/.test(v.lang) && /natural|neural|enhanced|premium|online/i.test(v.name),
    v => /en[-_]US/.test(v.lang) && /google/i.test(v.name),
    v => /en[-_]US/.test(v.lang) && /samantha|aria|jenny|guy/i.test(v.name),
    v => /en[-_]US/.test(v.lang),
    v => /^en/i.test(v.lang),
  ]
  for (const p of prefs){ const hit = voices.find(p); if (hit) return hit }
  return null
}
const LIAISON = {
  'want to':'wanna','going to':'gonna','have to':'hafta','has to':'hasta',
  'got to':'gotta','kind of':'kinda','sort of':'sorta','out of':'outta',
  'let me':'lemme','give me':'gimme','did you':'didja','would you':'wouldja',
  'meet you':'meetcha','pick it up':'pickitup','put it on':'putiton',
  'turn it off':'turnidoff','get up':'geddup','next day':'neksday',
}
function liaisoned(s){
  let t = ' ' + s.toLowerCase() + ' '
  Object.keys(LIAISON).sort((a,b)=>b.length-a.length).forEach(k=>{
    const pat = new RegExp('(?<=\\W)' + k.replace(/[.*+?^${}()|[\]\\]/g,'\\$&') + '(?=\\W)','g')
    t = t.replace(pat, LIAISON[k])
  })
  return t.trim()
}
function waveformsOn(){ document.querySelectorAll('.waveform').forEach(w => w.classList.add('playing')) }
function waveformsOff(){ document.querySelectorAll('.waveform').forEach(w => w.classList.remove('playing')) }
window.speakLine = function(text, rate, liaison){
  if (synth.speaking) synth.cancel()
  const u = new SpeechSynthesisUtterance(liaison ? liaisoned(text) : text)
  u.lang = 'en-US'; u.rate = rate || 1.0; u.pitch = 1.0
  const v = pickVoice(); if (v) u.voice = v
  u.onstart = waveformsOn
  u.onend   = waveformsOff
  u.onerror = waveformsOff
  // Some engines never fire onstart for short utterances; turn it on now too.
  waveformsOn()
  synth.speak(u)
}
const root = document.getElementById('app')
let model = root.dataset.model || ''
async function step(msg, opts){
  opts = opts || {}
  root.style.opacity = 0.55
  try {
    const url = '/api/step' + (msg ? ('?msg=' + encodeURIComponent(msg)) : '')
    const r = await fetch(url, { headers: { 'X-Model': model }, cache:'no-store' })
    if (!r.ok) throw new Error('HTTP '+r.status)
    model = r.headers.get('X-Model') || ''
    root.dataset.model = model
    root.innerHTML = await r.text()
    /- History API: if the server sent an `X-Url` header, push it
       into the address bar so the Back button replays the right
       state. `opts.replace` skips pushState — used when handling a
       popstate event so we don't churn the stack. -/
    const newUrl = r.headers.get('X-Url')
    if (newUrl && !opts.replace) {
      try { history.pushState({ model }, '', newUrl) } catch(e){}
    }
  } catch(e){ root.innerHTML = '<p style="color:#fca5a5">通信エラー: '+e.message+'</p>' }
  finally { root.style.opacity = 1 }
}

// Restore state on Back / Forward — re-issue the step with the same
// path so the server's `urlToMsg` derives the Msg fresh.
window.addEventListener('popstate', () => {
  const path = location.pathname + location.search
  step('__url__:' + path, { replace: true })
})
// ── Speech recognition (pronunciation check) ──
let recognizer = null
let micBusy = false
function normPron(s){
  return (s || '').toLowerCase().replace(/[^a-z\s']/g,'').replace(/\s+/g,' ').trim()
}
function strSim(a, b){
  if (!a || !b) return 0
  const longer = a.length > b.length ? a : b
  const shorter = a.length > b.length ? b : a
  if (longer.length === 0) return 1
  let cost = 0
  for (let i = 0; i < shorter.length; i++) if (shorter[i] !== longer[i]) cost++
  return (longer.length - cost) / longer.length
}
function scoreUtterance(target, said){
  const saidWords = normPron(said).split(' ')
  const targetWords = normPron(target).split(' ')
  let correct = 0, close = 0
  for (let i = 0; i < targetWords.length; i++){
    const w = targetWords[i]
    const sw = saidWords[i] || ''
    if (sw === w) correct++
    else if (strSim(sw, w) >= 0.6) close++
  }
  return Math.round((correct + close * 0.5) / targetWords.length * 100)
}
function renderColoredWords(target, said){
  const saidWords = normPron(said).split(' ')
  const targetTokens = target.split(' ')
  const targetWords = normPron(target).split(' ')
  return targetTokens.map((tok, i) => {
    const w = targetWords[i]
    const sw = saidWords[i] || ''
    let cls, mark
    if (sw === w){ cls = '#34d399'; mark = '✓' }
    else if (strSim(sw, w) >= 0.6){ cls = '#fbbf24'; mark = '△' }
    else { cls = '#f87171'; mark = '✗' }
    return `<span style="display:inline-block;padding:3px 7px;margin:3px;border-radius:6px;font-weight:600;background:${cls}22;border:1.5px solid ${cls};color:${cls}">${tok} <span style="font-size:0.75rem">${mark}</span></span>`
  }).join(' ')
}
function setMicState(btn, recording){
  btn.className = 'mic-btn ' + (recording ? 'recording' : 'idle')
  btn.textContent = recording ? '⏹' : '🎤'
}
function startMic(btn){
  const SR = window.SpeechRecognition || window.webkitSpeechRecognition
  const status = document.getElementById('pron-mic-status')
  const live = document.getElementById('pron-live')
  if (!SR){
    status.textContent = '⚠ このブラウザは SpeechRecognition 非対応（Chrome 推奨）'
    return
  }
  if (micBusy){ try{ recognizer && recognizer.stop() }catch(e){} ; return }
  const target = btn.dataset.pronTarget
  recognizer = new SR()
  recognizer.lang = 'en-US'
  recognizer.continuous = false
  recognizer.interimResults = true
  recognizer.maxAlternatives = 3
  let lastTranscript = ''
  micBusy = true
  setMicState(btn, true)
  status.textContent = '🔴 録音中... 発音してください'
  recognizer.onresult = e => {
    const parts = []
    for (let i = 0; i < e.results.length; i++) parts.push(e.results[i][0].transcript)
    lastTranscript = parts.join(' ').trim()
    live.innerHTML = renderColoredWords(target, lastTranscript) || '（認識中...）'
  }
  recognizer.onerror = e => {
    micBusy = false
    setMicState(btn, false)
    const msgs = {'not-allowed':'マイクの許可が必要です','no-speech':'音声が検出されませんでした','network':'ネットワークエラー'}
    status.textContent = '⚠ ' + (msgs[e.error] || 'エラー: ' + e.error)
  }
  recognizer.onend = () => {
    micBusy = false
    setMicState(btn, false)
    if (lastTranscript){
      const score = scoreUtterance(target, lastTranscript)
      status.textContent = '✅ 認識完了'
      // POST result back to the Lean side
      const body = 'score=' + encodeURIComponent(score) +
                   '&heard=' + encodeURIComponent(lastTranscript)
      step('pron-report:' + body)
    } else {
      status.textContent = '録音を停止しました'
    }
  }
  try { recognizer.start() } catch(e){
    micBusy = false; setMicState(btn, false)
    status.textContent = '⚠ 開始に失敗: ' + e.message
  }
}

document.addEventListener('click', e => {
  const tts = e.target.closest('[data-tts]')
  if (tts) {
    e.preventDefault()
    window.speakLine(
      tts.dataset.tts,
      parseFloat(tts.dataset.rate || '0.95'),
      tts.dataset.liaison === '1',
    )
    return
  }
  const mic = e.target.closest('[data-mic]')
  if (mic) {
    e.preventDefault()
    startMic(mic)
    return
  }
  const a = e.target.closest('[data-msg]')
  if (!a) return
  // Forms own their data-msg for the submit handler — don't fire it
  // from a click inside the form (e.g. focusing an <input>), which
  // would submit an empty answer before the user typed anything.
  if (a.tagName === 'FORM') return
  e.preventDefault()
  step(a.dataset.msg)
})
document.addEventListener('submit', e => {
  const f = e.target.closest('form[data-msg]')
  if (!f) return
  e.preventDefault()
  const msg = f.dataset.msg || ''
  const fd = new FormData(f)
  const parts = []
  for (const [k,v] of fd) parts.push(k + '=' + encodeURIComponent(v))
  step(msg + (parts.length ? ':' + parts.join('&') : ''))
})
})()
