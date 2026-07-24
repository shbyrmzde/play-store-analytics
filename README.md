# play-store-analytics
Data cleaning and exploratory analysis of Google Play Store apps (R, dplyr, Shiny) — category insights for an ad-supported app business.
# Play Store Apps Analytics

**Problem:** This project demonstrates data cleaning and analysis skills for
a junior data analyst portfolio, framed around a real business question: which
app categories should an ad-supported mobile app company prioritize?

**Data:** Google Play Store Apps dataset (Kaggle, ~10,000 apps, publicly
available), containing category, rating, installs, price, and update history.
The raw data required extensive cleaning — inconsistent formats, duplicates,
misaligned rows, and locale-dependent date parsing bugs.

**Findings:** GAME leads in total installs but is highly saturated (946
competing apps). Categories like VIDEO_PLAYERS and ENTERTAINMENT show high
installs-per-app with far less competition. 92% of apps are Free, confirming
ad-supported monetization dominates the market. Rating and Reviews count are
only weakly correlated (r = 0.055) — popularity doesn't guarantee quality.

**Recommendation:** Target low-competition, high-demand categories rather
than saturated ones, maintain the free/ad-supported model, and prioritize
Rating over Reviews when evaluating app quality.

## Links

- 📊 Full analysis (static HTML): [RPubs](http://rpubs.com/sheelaxw/1448895)
- 🚀 Interactive dashboard: [shinyapps.io](https://shalalabayramova.shinyapps.io/apps/)
- 🎥 Video walkthrough: on LinkedIn

## Tech Stack

R, dplyr, stringr, plotly, shiny, shinydashboard, shinyWidgets
