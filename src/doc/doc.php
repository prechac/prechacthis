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
	echo "<div class='doc_close'><a href='../index.php' target='_parent'>close</a>";
	echo "&nbsp;|&nbsp;<a href='./doc.php' target='Doc'>up</a>";
	//$filename = "./sections/".$_REQUEST['section']."inc";
	$filename = $_REQUEST['section'];
	$file = fopen($filename,"r");
	$doc_title = fgets($file); // Read first line
	$doc_url = fgets($file); // Read second line
	echo "<h1>".$doc_title."</h1>";
	echo fpassthru($file);
	fclose($file);
} else {
	echo "<div class='doc_close'><a href='../index.php' target='_parent'>close</a></div>";
	echo "<h1>Documentation</h1>";
	echo "<ul>\n";
	foreach (glob("./sections/*.inc") as $filename) {
		$file = fopen($filename,"r");
		$doc_title = fgets($file); // Read first line
		$doc_url = "./doc.php?section=". $filename;
		$doc_url_encoded = rawurlencode($doc_url);
		$pt_url = fgets($file); // Read second line
		$pt_url_encoded = rawurlencode($pt_url);
	
		echo "<li><a href='./index.php?docurl=".$doc_url_encoded."&pturl=".$pt_url_encoded."' target='_parent'>". $doc_title ."</a></li>"; 
	
		fclose($file);
	}
	echo "</ul>\n";
}
?>



	</body>
</html>
