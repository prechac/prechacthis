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
if ($_REQUEST['showedit'] == "on") {
	$DocEndSolo = "?showedit=on";
	$DocEnd = "&showedit=on";
} else {
	$DocEndSolo = "";
	$DocEnd = "";
}
if (isset($_REQUEST['section'])) {
	if ($_REQUEST['edit'] == "on") {
		echo "<div class='back'><a href='../index.php' target='_parent'>close</a>";
		echo "&nbsp;|&nbsp;<a href='./doc.php". $DocEndSolo ."' target='Doc'>up</a>";
		echo "</div>\n";
		$filename = $_REQUEST['section'];
		$file = fopen($filename,"r");
		echo "<form action='./doc.php' target='Doc' method='post'>";
		echo "<input type='hidden' name='section' value='".$_REQUEST['section']."'>";
		echo "<input type='hidden' name='showedit' value='on'>";
		echo "<p>";
	    echo "<textarea cols='40' rows='40' name='sectioncontent' wrap='off'>";
		fpassthru($file);
		echo "</textarea>";
		echo "</p>";
		echo "<p>";
		echo "<input type='submit' value='save'>";
		echo "</p>";
		echo "</form>";
		fclose($file);
	} else {
		//$filename = "./sections/".$_REQUEST['section']."inc";
		$filename = $_REQUEST['section'];
		if (isset($_REQUEST['sectioncontent'])) {
			$file = fopen($filename,"w");
			
			fclose($file);
		}
		echo "<div class='back'><a href='../index.php' target='_parent'>close</a>";
		echo "&nbsp;|&nbsp;<a href='./doc.php". $DocEndSolo ."' target='Doc'>up</a>";
		if ($_REQUEST['showedit'] == "on") {
			echo "&nbsp;|&nbsp;<a href='./doc.php?edit=on&section=".$_REQUEST['section']."' target='Doc'>edit</a>";
		}
		echo "</div>\n";
		$file = fopen($filename,"r");
		$doc_title = fgets($file); // Read first line
		$doc_url = fgets($file); // Read second line
		echo "<h1>".$doc_title."</h1>\n";
		echo "<div id='doc_main'>\n";
		fpassthru($file);
		echo "</div>\n";
		fclose($file);
	}
} else {
	echo "<div class='back'><a href='../index.php' target='_parent'>close</a></div>\n";
	echo "<h1>Documentation</h1>\n";
	echo "<div id='doc_main'>\n";
	echo "<ul>\n";
	foreach (glob("./sections/*.inc") as $filename) {
		$file = fopen($filename,"r");
		$doc_title = fgets($file); // Read first line
		$doc_url = "./doc.php?section=". $filename . $DocEnd;
		$doc_url_encoded = rawurlencode($doc_url);
		$pt_url = fgets($file); // Read second line
		$pt_url_encoded = rawurlencode($pt_url);
		
		echo "<li><a href='./index.php?docurl=".$doc_url_encoded."&pturl=".$pt_url_encoded."' target='_parent'>". $doc_title ."</a></li>\n"; 
	
		fclose($file);
	}
	echo "</ul>\n";
	echo "</div>\n";
	echo "<div class='back'><a href='../index.php' target='_parent'>close</a></div>\n";
}
?>

</body>
</html>
