Shiny.renderFlagOption = function(item) {
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

document.addEventListener("shiny:connected", function() {
  const languages = navigator.languages || [navigator.language || ""];
  const browserLang = (languages[0] || "").toLowerCase();

  Shiny.setInputValue("browser_lang", browserLang, { priority: "event" });
});
