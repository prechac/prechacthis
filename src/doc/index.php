<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd">
<html>
	<head>
		<title>
			PrechacThis - Documentation
		</title>
		<link rel="shortcut icon" href="../images/favicon.png">
	</head>
	<frameset cols="350,*">
	
		<?php
			if(isset($_REQUEST["pturl"])) {
				$pt_encoded = $_REQUEST["pturl"];
				$pt_url = rawurldecode($pt_encoded);
			}else{
				$pt_url = "index.php";
			}
			if(isset($_REQUEST["docurl"])) {
				$doc_encoded = $_REQUEST["docurl"];
				$doc_url = rawurldecode($doc_encoded);			
			}else{
				$doc_url = "doc.php";
			}
			if($_REQUEST["showedit"] == "on") {
				$doc_url = $doc_url . "&showedit=on";			
			}
			echo "<frame src='./".$doc_url."' name='Doc'>";
			echo "<frame src='../". $pt_url ."' name='PrechacThis'>";
		?>
		
		<noframes>
			<body>
				<p>
					<a href="./doc.php">Documentation</a> <a href="../index.php">PrechacThis</a>
				</p>
			</body>
		</noframes>
	</frameset>
</html>
