document.addEventListener('DOMContentLoaded', function() {
  // Fetch Datamall data via fly.io
  Shiny.addCustomMessageHandler('fetch_datamall', function(params) {
    document.getElementById('train_info_out').innerHTML =
      '<span style=\"color:#2050C0; font-weight:bold;\"><i class=\"fas fa-hourglass-half\"></i> Getting latest train info, please wait...</span>';
    const train_info_url = 'https://stcraft.myddns.me/datamall-proxy' +
      '?account_key=default_key_1' +
      '&data_type=train_status';
    fetch(train_info_url)
      .then(response => {
        if (!response.ok) {
          return response.json().then(text => {
            throw new Error(
              `${text.error}`
            );
          });
        }
        return response.text();
      })
      .then(function(train_info) {
        // Pass the CSV text to Shiny.
        Shiny.setInputValue('train_info_in', train_info)
        // document.getElementById('train_info_out').innerHTML = train_info;
      })
      .catch(err => {
        console.error(err);
        document.getElementById('train_info_out').innerHTML =
          '<span style="color:#BB0000; font-weight:bold;"><i class="fas fa-triangle-exclamation"></i> ' + err.message + '</span>';
      });
  });

  // Fetch JSON data (data2 & data3) from BusRouter.
  Shiny.addCustomMessageHandler('fetch_sheet', function(params) {
    return fetch("https://stcraft.myddns.me/repository/report-sheet").then(function(res) {
      return res.json().then(function(sheet) {
      console.log(sheet)
      Shiny.setInputValue('sheets_in', JSON.stringify(sheet));
    })})
  });

  // Clear cache upon refresh
  window.onbeforeunload = function() {
    // Clear session storage
    sessionStorage.clear();
    // Clear local storage (or specific cache keys)
    localStorage.clear();
    // Clear caches created via the Cache API
    if ('caches' in window) {
      caches.keys().then(function(names) {
        names.forEach(function(name) {
          caches.delete(name);
        });
      });
    }
  };
})