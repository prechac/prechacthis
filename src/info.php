<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
	<title>PrechacThis - Pattern Info</title>
	<link rel="shortcut icon" href="./favicon.png">
	<link rel="stylesheet" type="text/css" href="./css/prechacthis.css">
</head>
<body>
<?php
$debug = false;

if ($_GET){
	
	//foreach($_GET as $oneGET) {
	//	echo "<p>$oneGET</p>";
	//}
	
	if($_GET["swap"]) {
		$swaplist = $_GET["swap"];
	}else{
		$swaplist = "[]";
	}
	
	if($_GET["newswap"]) {
		$newswap = $_GET["newswap"];
	} else {
		$newswap = "[]";
	}
	
	if($_GET["debug"]=="on") $debug = true;

	if($_GET["back"]) {
		$back_url = $_GET["back"];
		$back_url_decoded = rawurldecode($back_url);
		$back_url_encoded = rawurlencode($back_url_decoded);
		
		echo "<p class='back'><a href='./?".$back_url_decoded."'>back to results</a></p>";
	}
	
	
	if($_GET["pattern"] && $_GET["persons"]) {
	    echo "\n<table align='center' cellpadding='0'><tr><td><div align='center'>\n";

		$errorlogfile = tempnam("/tmp", "siteswap_info");

		$plquery  = "swipl -q "
		          . "-f " . dirname($_SERVER["SCRIPT_FILENAME"]) . "/pl/siteswap.pl "
		          . "-g \"print_pattern_info("
		          . $_GET["pattern"] . ", "
		          . $_GET["persons"] . ", "
				  . $swaplist . ", "
				  . $newswap . ","
		          . "'$back_url_encoded'"
		          . "), halt.\" "
		          . "2> $errorlogfile";

		if ($debug) echo "$plquery<br>\n";
		echo `$plquery`;
		readfile($errorlogfile);
		unlink($errorlogfile);

		echo "\n</div></td></tr></table>";
	}else{
		echo "<p>pattern or number of jugglers not specified :-(</p>";
	}
	
	if($_GET["back"]) {
		echo "<br><p class='back'><a href='./?".$back_url_decoded."'>back to results</a></p>";
	}
}
?>

</body>
</html>
