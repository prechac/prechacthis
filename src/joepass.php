<?php
if (isset($_GET["pattern"]) && isset($_GET["persons"])){
	
	if(isset($_GET["swap"])) {
		$swaplist = $_GET["swap"];
	}else{
		$swaplist = "[]";
	}
	
	$plquery  = "swipl -q "
	          . "-f " . dirname($_SERVER["SCRIPT_FILENAME"]) . "/pl/siteswap.pl "
	          . "-g \"jp_pattern_def("
	          . $_GET["pattern"] . ", "
	          . $_GET["persons"] . ", "
			  . $swaplist
	          . "), halt.\"";
	
	if(isset($_GET["file"])) {
    	header("Content-type: application/force-download");
    	header("Content-Transfer-Encoding: Binary");
    	header("Content-disposition: attachment; filename=\"".$_REQUEST["persons"]."_".$_REQUEST["file"].".pass\"");
	} else {
	    header("Content-type: text/plain");
	}
	if(isset($_GET["debug"])) {
		echo $plquery;
	}
	echo `$plquery`;
} else {
	echo "something is wrong :-(";
}
?>