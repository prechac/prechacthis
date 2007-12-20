<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
	<title>PrechacThis - Pattern Info</title>
	<link rel="shortcut icon" href="favicon.png">
	<link rel="stylesheet" type="text/css" href="./prechacthis.css">
</head>
<body>
<div id="beta">this page is still beta!</div>
<?php
$debug = false;

if ($_REQUEST){
	if($_GET["debug"]=="on") $debug = true;

	
	if($_GET["pattern"] && $_GET["persons"]) {
	    echo "\n<table align='center' cellpadding='0'><tr><td><div align='center'>\n";

		$errorlogfile = tempnam("/tmp", "siteswap_info");

		$plquery  = "swipl -q "
		          . "-f " . dirname($_SERVER["SCRIPT_FILENAME"]) . "/pl/siteswap.pl "
		          . "-g \"print_pattern_info("
		          . $_GET["pattern"] 
				  . ", "
		          . $_GET["persons"]
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
}
?>

</body>
</html>