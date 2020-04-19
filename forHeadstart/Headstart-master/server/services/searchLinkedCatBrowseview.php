<?php

header('Content-type: application/json');

require_once dirname(__FILE__) . '/../classes/headstart/library/CommUtils.php';
require 'search.php';

use headstart\library;

$dirty_query = library\CommUtils::getParameter($_POST, "q");

$post_params = $_POST;

$result = search("linkedcat_browseview",
                 $dirty_query,
                 $post_params,
                 array("today", "bkl_level", "bkl_list", "doc_count", "bkl_top_caption", "from", "to", "include_content_type"),
                 ";",
                 null,
                 $transform_query_tolowercase = false
               );

echo $result

?>
