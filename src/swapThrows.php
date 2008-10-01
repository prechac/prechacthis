<?php
	if(isset($_REQUEST["pattern"]) && isset($_REQUEST["pos1"]) && isset($_REQUEST["pos2"])) {

		$errorlogfile = tempnam("/tmp", "siteswap_info");
	
		$plquery  = "swipl -q "
		          . "-f " . dirname($_SERVER["SCRIPT_FILENAME"]) . "/pl/siteswap.pl "
		          . "-g \"print_swaped_pattern("
		          . $_REQUEST["pattern"] . ", "
		          . $_REQUEST["pos1"] . ", "
		          . $_REQUEST["pos2"]
		          . "), halt.\" "
		          . "2> $errorlogfile";

		echo `$plquery`;
		readfile($errorlogfile);
		unlink($errorlogfile);
	} else {
		echo "-1000";
	}
?>