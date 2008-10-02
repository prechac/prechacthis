<?php
if (isset($_REQUEST["pattern"]) && isset($_REQUEST["persons"])){
	
	if(isset($_REQUEST["swap"])) {
		$swaplist = $_REQUEST["swap"];
	}else{
		$swaplist = "[]";
	}	
	if(isset($_REQUEST["download"])) {
		$download = $_REQUEST["download"];
		setcookie("joepass_download", $_REQUEST["download"], time()+60*60*24*365*10);
	}else{
		$download = "on";
	}
	if(isset($_REQUEST["style"])) {
		$style = $_REQUEST["style"];
		setcookie("joepass_style", $_REQUEST["style"], time()+60*60*24*365*10);
	}else{
		$style = "none";
	}
	if ( $_REQUEST["filename"] == "joe" || !isset($_REQUEST["file"])) {	
		$filename = "joe.pass";
		setcookie("joepass_file", "joe", time()+60*60*24*365*10);
	} else {
		$filename = $_REQUEST["persons"]."_".$_REQUEST["file"].".pass";
		setcookie("joepass_file", "numbers", time()+60*60*24*365*10);
	}
	$plquery  = "swipl -q "
	          . "-f " . dirname($_SERVER["SCRIPT_FILENAME"]) . "/pl/siteswap.pl "
	          . "-g \"jp_pattern_def("
	          . $_REQUEST["pattern"] . ", "
	          . $_REQUEST["persons"] . ", "
			  . $swaplist. ", "
			  . $style
	          . "), halt.\"";
	
	
	if(($download == 'on') && isset($_REQUEST["file"])) {
    	header("Content-type: application/force-download");
    	header("Content-Transfer-Encoding: Binary");
    	header("Content-disposition: attachment; filename=\"".$filename."\"");
		echo `$plquery`;
	} else {		
?><!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
	<title>PrechacThis - JoePass</title>
	<link rel="shortcut icon" href="./images/favicon.png">
	<link rel="stylesheet" type="text/css" href="./css/prechacthis.css">
</head>
<body>
<?php
		if(isset($_REQUEST["debug"])) {
			echo "<pre>". $plquery ."</pre>";
		}
?>
	<pre><?php echo `$plquery`; ?></pre>
	<br>
	<br>
<?php 
	echo "<div class='jp_backlink'>";
	//echo "<a href=\"./joepass.php?download=on&". $_SERVER["QUERY_STRING"] ."\">download</a>"; 
	if(isset($_SERVER["HTTP_REFERER"])) {echo "<a href=\"". $_SERVER["HTTP_REFERER"] ."\">back</a>";} 
	echo "</div>";
?>

</body>
</html>	
	
<?php
	}
} else {
	echo "something is wrong :-(";
}
?>