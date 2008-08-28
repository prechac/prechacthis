<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN"
"http://www.w3.org/TR/html4/strict.dtd">
<html>
	<head>
		<title>
			PrechacThis - Documentation
		</title>
		<link rel="shortcut icon" href="../images/favicon.png">
	</head>
	<frameset cols="300,*">
	
		<?php
		if(isset($_REQUEST["pturl"])) {
			$pt_url = $_REQUEST["pturl"];
		}else{
			$pt_url = "../index.php";
		}
		if(isset($_REQUEST["docurl"])) {
			$doc_url = $_REQUEST["docurl"];
		}else{
			$doc_url = "";
		}
		echo "<frame src='./doc.php?".$doc_url."' name='Docu'>";
		echo "<frame src='". $pt_url ."' name='PrechacThis'>";
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
