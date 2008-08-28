<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd">
<html>
	<head>
		<title>
			PrechacThis - Documentation
		</title>
		<link rel="stylesheet" type="text/css" href="../css/prechacthis.css">
	</head>
	<body>
	
<?php
if (isset($_REQUEST['section'])) {
	//$filename = "./sections/".$_REQUEST['section']."inc";
	$filename = $_REQUEST['section'];
	$file = fopen($filename,"r");
	$doc_title = fgets($file); // Read first line
	$doc_url = fgets($file); // Read second line
	echo "<h1>".$doc_title."</h1>";
	echo fpassthru($file);
	fclose($file);
} else {
?>
		<h1>Documentation</h1>
<?php
	echo "<ul>\n";
	foreach (glob("./sections/*.doc") as $filename) {
		$file = fopen($filename,"r");
		$doc_title = fgets($file); // Read first line
		$doc_url = fgets($file); // Read second line
	
		echo "<li><a href='./index.php?docurl=".$doc_url."&pturl=".$pt_url."' target='_parent'>". $doc_title ."</a></li>"; 
	
		fclose($file);
	}
	echo "</ul>\n";
}
?>



	</body>
</html>
