window.Shiny = window.Shiny || {};

window.Shiny.renderFlagOption = function(item) {
  const lang = item.value;

  const flags = {
    no: "🇳🇴",
    nn: "🇳🇴",
    en: "🇬🇧"
  };

  const names = {
    no: "Bokmål",
    nn: "Nynorsk",
    en: "English"
  };

  const flag = flags[lang] || "";
  const label = names[lang] || lang;

  return `
    <div style="display:flex; align-items:center; gap:8px;">
      <span style="font-size:1.1em;">${flag}</span>
      <span>${label}</span>
    </div>
  `;
};

function sendBrowserLanguageToShiny() {
  if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") {
    return false;
  }

  const languages = navigator.languages || [navigator.language || ""];
  const browserLang = (languages[0] || "").toLowerCase();

  window.Shiny.setInputValue("custom_js_loaded", true, { priority: "event" });
  window.Shiny.setInputValue("browser_lang", browserLang, { priority: "event" });
  window.Shiny.setInputValue("browser_langs", languages.join(", "), { priority: "event" });
  return true;
}

function scheduleBrowserLanguageRetry(maxAttempts = 20, delayMs = 250) {
  let attempts = 0;

  function trySend() {
    attempts += 1;

    if (sendBrowserLanguageToShiny() || attempts >= maxAttempts) {
      return;
    }

    window.setTimeout(trySend, delayMs);
  }

  trySend();
}

document.addEventListener("DOMContentLoaded", function() {
  scheduleBrowserLanguageRetry();
});

document.addEventListener("shiny:connected", function() {
  scheduleBrowserLanguageRetry();
});

if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === "function") {
  window.Shiny.addCustomMessageHandler("redirect-to-url", function(message) {
    if (message && message.url) {
      window.location.href = message.url;
    }
  });
}
