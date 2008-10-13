<?php
	if($_REQUEST["back"]) {
		$back_url = $_REQUEST["back"];
		$back_url_decoded = rawurldecode($back_url);
		$back_url_encoded = rawurlencode($back_url_decoded);
	}
	if (!isset($_REQUEST["ajax"])) {
?>		
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
	<title>PrechacThis - Pattern Info</title>
	<link rel="shortcut icon" href="./images/favicon.png">
	<link rel="stylesheet" type="text/css" href="./css/prechacthis.css">
	<meta http-equiv="Content-Type" content="text/html;charset=utf-8">
	<script type="text/javascript" src="./js/prototype/prototype.js"></script>
	<script type="text/javascript" src="./js/scriptaculous/scriptaculous.js"></script>
	<script type="text/javascript" src="./js/ajax.js"></script>
</head>
<body>
<?php
	if($_REQUEST["back"]) {
		echo "<p class='back'><a href='./?".$back_url_decoded."'>back to results</a></p>";
	}	
?>
	<table align='center' cellpadding='0'><tr><td><div id='content' align='center'>
<?php

}
$debug = false;

if ($_REQUEST){
	
	//foreach($_REQUEST as $oneGET) {
	//	echo "<p>$oneGET</p>";
	//}
	
	if($_REQUEST["swap"]) {
		$swaplist = $_REQUEST["swap"];
	}else{
		$swaplist = "[]";
	}
	
	if($_REQUEST["newswap"]) {
		$newswap = $_REQUEST["newswap"];
	} else {
		$newswap = "[]";
	}
	
		
	if($_REQUEST["hreftype"] == "ajax") {
		$hreftype = $_REQUEST["hreftype"];
	} else {
		$hreftype = "html";
	}
	
	$joepass_cookies = "[";
	if(isset($_COOKIE["joepass_download"])) {
		$joepass_cookies .= "'".$_COOKIE["joepass_download"]."'";
	} else {
		$joepass_cookies .= "''";
	}
	if(isset($_COOKIE["joepass_style"])) {
		$joepass_cookies .= ", '".$_COOKIE["joepass_style"]."'";
	} else {
		$joepass_cookies .= ", ''";
	}
	if(isset($_COOKIE["joepass_file"])) {
		$joepass_cookies .= ", '".$_COOKIE["joepass_file"]."'";
	} else {
		$joepass_cookies .= ", ''";
	}
	$joepass_cookies .= "]";
	//echo $joepass_cookies;
	
	if($_REQUEST["debug"]=="on") $debug = true;

	//$browser = get_browser();
	//echo ($browser['javascript']);
	
	if($_REQUEST["pattern"] && $_REQUEST["persons"]) {

		$errorlogfile = tempnam("/tmp", "siteswap_info");
		
		$plquery  = "swipl -q "
		          . "-f " . dirname($_SERVER["SCRIPT_FILENAME"]) . "/pl/siteswap.pl "
		          . "-g \"print_pattern_info("
		          . $_REQUEST["pattern"] . ", "
		          . $_REQUEST["persons"] . ", "
				  . $swaplist . ", "
				  . $newswap . ", "
				  . $hreftype . ", "
				  . $joepass_cookies . ", "
		          . "'$back_url_encoded'"
		          . "), halt.\" "
		          . "2> $errorlogfile";

		if ($debug) echo "$plquery<br>\n";
		echo `$plquery`;
		readfile($errorlogfile);
		unlink($errorlogfile);
	} else {
		echo "<p>pattern or number of jugglers not specified!</p>";
	}
	
}	
	
if (!isset($_REQUEST["ajax"])) {
	
	echo "\n</div></td></tr></table>\n";	
	echo "<br><p class='back'>";
	if($_REQUEST["back"]) {
		echo "<a href='./?".$back_url_decoded."'>back to results</a>";
	}
	echo "<span id='linkhere'></span>";
	echo "</p>";
?>	
<script type="text/javascript" src="./js/swap.js"></script>
</body>
</html>
<?php
}
?>