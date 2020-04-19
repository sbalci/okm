<!DOCTYPE html>
<?php
include 'config.php';
?>
<html>

<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <title>CRIS Vis II</title>
    <link rel="stylesheet" href="https://use.typekit.net/ift7wkn.css"></link>
    <link href='https://fonts.googleapis.com/css?family=PT+Sans:400,700' rel='stylesheet' type='text/css'></link>
</head>

<body style="margin:0px; padding:0px; height:100%; position: relative;">
    <div id="visualization" class="headstart"></div>
    <script type="text/javascript" src="data-config2_de.js"></script>
    <script type="text/javascript" src="<?php echo $HEADSTART_PATH ?>dist/headstart.js"></script>
    <link type="text/css" rel="stylesheet" href="<?php echo $HEADSTART_PATH ?>dist/headstart.css"></link>
    <script type="text/javascript">
        data_config.server_url = window.location.href.replace(/[^/]*$/, '') + "../../server/";
        headstart.start();
    </script>
    
    <!-- Piwik -->
    <script type="text/javascript">
      var _paq = _paq || [];
      // tracker methods like "setCustomDimension" should be called before "trackPageView"
      _paq.push(['trackPageView']);
      _paq.push(['enableLinkTracking']);
      (function() {
        var u="https://openknowledgemaps.org/piwik_stats/";
        _paq.push(['setTrackerUrl', u+'piwik.php']);
        _paq.push(['setSiteId', '1']);
        var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
        g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'piwik.js'; s.parentNode.insertBefore(g,s);
      })();
    </script>
    <!-- End Piwik Code -->
</body>

</html>
