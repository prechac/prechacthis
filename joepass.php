<?php
if (isset($_GET["pattern"]) && isset($_GET["persons"])){
	
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
	
	$plquery  = "swipl -q "
	          . "-f " . dirname($_SERVER["SCRIPT_FILENAME"]) . "/pl/siteswap.pl "
	          . "-g \"print_pattern_info("
	          . $_GET["pattern"] . ", "
	          . $_GET["persons"] . ", "
			  . $swaplist . ", "
			  . $newswap . ", "
			  . "''"
	          . "), halt.\"";

    header("Content-type: application/force-download");
    header("Content-Transfer-Encoding: Binary");
    header("Content-disposition: attachment; filename=\"".$_REQUEST["pattern"].".pass\"");
	echo `$plquery`;
}
?>