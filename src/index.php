<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
	<title>PrechacThis</title>
	<link rel="shortcut icon" href="./images/favicon.png">
	<link rel="stylesheet" type="text/css" href="./css/prechacthis.css">
</head>
<body>

<?php
$debug = false;

if ($_REQUEST){
	if($_GET["debug"]=="on") $debug = true;
	
	$doku = doku($_GET["doku"]);
	if($doku){
?>
	<table align='center' cellpadding='0'>
		<tr>
			<td class='classictd'>&nbsp;</td><td>Classic</td>
			<td>&nbsp;-&nbsp;</td>
			<td class='equitd'>&nbsp;</td><td>Equihanded</td>
			<td>&nbsp;-&nbsp;</td>
			<td class='bitd'>&nbsp;</td><td>Bi</td>
			<td>&nbsp;-&nbsp;</td>
			<td class='instantbitd'>&nbsp;</td><td>Instant Bi</td>
		</tr>
	</table><br>
<?php
	}

    echo "<table align='center' cellpadding='0'><tr><td>";

	if (!$_GET["objects"]){$_GET["objects"] = "3;4;5;6;7";}
	$_GET["objects"] = str_replace(";", "or", $_GET["objects"]);
	$objects = explode("or", $_GET["objects"]);
	
	if (!$_GET["lengths"]){$_GET["lengths"] = "2;3;4;5;6;8";}
	$_GET["lengths"] = str_replace(";", "or", $_GET["lengths"]);
	$lengths = explode("or", $_GET["lengths"]);
	
	if ($_GET["persons"]){echo "<h2>" . $_GET["persons"] . " persons</h2>";}

	if (!$_GET["multiplex"]){$_GET["multiplex"] = "0";}

	if (!$_GET["results"]){$_GET["results"] = 42;}
	
	$back_url = rawurlencode($_SERVER["QUERY_STRING"]);
	
	foreach($objects as $object){
		foreach($lengths as $length){
			$errorlogfile = tempnam("/tmp", "siteswap");
	
			$plquery  = "swipl -q "
					  . "-L5M " // local stack size to 5 MB
					  . "-G12M " // global stack size to 12 MB
					  . "-T5M " // trail stack size to 5 MB
					  . "-A2M " // argutment stack size to 2 MB
			          . "-f " . dirname($_SERVER["SCRIPT_FILENAME"]) . "/pl/siteswap.pl "
			          . "-g \"allSiteswaps("
			          . $_GET["persons"] . ", "
			          . "$object, "
			          . "$length, "
			          . $_GET["max"] . ", "
			          . $_GET["multiplex"] . ", "
			          . correctPasses($_GET["passesmin"]) . ", "
			          . correctPasses($_GET["passesmax"]) . ", "
			          . "\\\"". $_GET["jugglerdoes"] . "\\\", "
			          . "\\\"". $_GET["exclude"] . "\\\", "
			          . "\\\"". $_GET["clubdoes"] . "\\\", "
			          . "\\\"". $_GET["react"] . "\\\", "
					  . $_GET["results"] . ", "
			          . "'". $back_url ."'"
			          . "), halt.\" "
			          . "2> $errorlogfile";
	
			echo "<div class='inline'><div class='swaps'>";
			echo "<h3>$object objects, ";
			echo "period $length:</h3>";
			if ($debug) echo "$plquery<br>\n";
			echo `$plquery`;
			readfile($errorlogfile);
			unlink($errorlogfile);
			echo "</div></div>";
			
		}//lengths
	}//objects
	echo "</td></tr></table>";
}

//default values
if (!$_GET["persons"]){$_GET["persons"] = 2;}
$personsSelected[$_GET["persons"]] = "selected='selected'";

if (!$_GET["multiplex"]){$_GET["multiplex"] = 0;}
$multiplexSelected[$_GET["multiplex"]] = "selected='selected'";

if (!$_GET["max"]){$_GET["max"] = 4;}
$maxSelected[$_GET["max"]] = "selected='selected'";

if (!$_GET["passesmin"]){$_GET["passesmin"] = 1;}
$minPassSelected[$_GET["passesmin"]] = "selected='selected'";

if (!$_GET["passesmax"] or !is_numeric($_GET["passesmax"])){$_GET["passesmax"] = 0;}
$maxPassSelected[$_GET["passesmax"]] = "selected='selected'";

$hidden_debug="";
if ($debug) $hidden_debug = "<input type='hidden' name='debug' value='on'>";

echo "<form action='./index.php' method='get'>
 <table align='center' cellpadding='10'>
  <tr>
   <td class='lable'></td>
   <td class='input'></td>
   $doku[head]
  </tr>
  <tr>
   <td class='lable'>Jugglers:</td>
   <td class='input'>
    <select name='persons' size='1'>
     <option $personsSelected[1]>1</option>
     <option $personsSelected[2]>2</option>
     <option $personsSelected[3]>3</option>
     <option $personsSelected[4]>4</option>
     <option $personsSelected[5]>5</option>
     <option $personsSelected[6]>6</option>
     <option $personsSelected[7]>7</option>
     <option $personsSelected[8]>8</option>
     <option $personsSelected[9]>9</option>
     <option $personsSelected[10]>10</option>
    </select>
   </td>
   $doku[persons]
  </tr>
  <tr>
   <td class='lable'>Objects:</td>
   <td class='input'><input type='text' name='objects' value='$_GET[objects]'></td>
   $doku[objects]
  </tr>
  <tr>
   <td class='lable'>Period:</td>
   <td class='input'><input type='text' name='lengths' value='$_GET[lengths]'></td>
   $doku[length]
  </tr>
  <tr>
   <td class='lable'>Max height:</td>
   <td class='input'>
    <select name='max' size='1'>
     <option $maxSelected[1]>1</option>
     <option $maxSelected[2]>2</option>
     <option $maxSelected[3]>3</option>
     <option $maxSelected[4]>4</option>
     <option $maxSelected[5]>5</option>
     <option $maxSelected[6]>6</option>
     <option $maxSelected[7]>7</option>
     <option $maxSelected[8]>8</option>
    </select>
   </td>
   $doku[max]
  </tr>
  <tr>
   <td class='lable'>Passes</td>
   <td class='input'>
    min:
    <select name='passesmin' size='1'>
     <option $minPassSelected[0]>0</option>
     <option $minPassSelected[1]>1</option>
     <option $minPassSelected[2]>2</option>
     <option $minPassSelected[3]>3</option>
     <option $minPassSelected[4]>4</option>
     <option $minPassSelected[5]>5</option>
     <option $minPassSelected[6]>6</option>
     <option $minPassSelected[7]>7</option>
     <option $minPassSelected[8]>8</option>
    </select>
    max:
    <select name='passesmax' size='1'>
     <option $maxPassSelected[1]>1</option>
     <option $maxPassSelected[2]>2</option>
     <option $maxPassSelected[3]>3</option>
     <option $maxPassSelected[4]>4</option>
     <option $maxPassSelected[5]>5</option>
     <option $maxPassSelected[6]>6</option>
     <option $maxPassSelected[7]>7</option>
     <option $maxPassSelected[8]>8</option>
     <option $maxPassSelected[0]>&nbsp;</option>
    </select>
   </td>
  </tr>".
/*  <tr>
   <td class='lable'>Multiplexes:</td>
   <td class='input'>
    <select name='multiplex' size='1'>
     <option $multiplexSelected[0]>0</option>
     <option $multiplexSelected[1]>1</option>
     <option $multiplexSelected[2]>2</option>
     <option $multiplexSelected[3]>3</option>
     <option $multiplexSelected[4]>4</option>
    </select>
   </td>
   $doku[multiplex]
  </tr>*/
  "<tr>
   <td class='lable'>Contain:</td>
   <td class='input'><input type='text' name='jugglerdoes' value='$_GET[jugglerdoes]'></td>
   $doku[contain]
  </tr>
  <tr>
   <td class='lable'>Exclude:</td>
   <td class='input'><input type='text' name='exclude' value='$_GET[exclude]'></td>
   $doku[exclude]
  </tr>
  <tr>
   <td class='lable'>Club does:</td>
   <td class='input'><input type='text' name='clubdoes' value='$_GET[clubdoes]'></td>
   $doku[clubdoes]
  </tr>
  <tr>
   <td class='lable'>React:</td>
   <td class='input'><input type='text' name='react' value='$_GET[react]'></td>
   $doku[react]
  </tr>
  <tr>
   <td class='lable'>Max results:</td>
   <td class='input'><input type='text' name='results' value='$_GET[results]'></td>
   $doku[results]
  </tr>

  <tr>
   <td class='lable'>&nbsp;</td>
   <td class='input'><input type='submit' value='Generate' ></td>
  </tr>
  <tr>
   <td class='lable'>&nbsp;</td>
   <td class='input'><a href='./index.php?persons=2&objects=4%3B5&lengths=4%3B5%3B6&max=4&passesmin=1&passesmax=4&jugglerdoes=3+%283p+or+4p%29+or+3+%3Fp&exclude=0+and+1+1+and+2+2&clubdoes=%281+or+2%29+%281p+or+1.5p%29+or+%281p+or+1.5p%29+%281+or+2%29&react=2p+3+or+4p+4+or+3+1.5p&results=42&doku=on'>documented example</a></td>
  </tr>
  <tr>
   <td class='lable'>&nbsp;</td>
   <td class='input'><a href='http://sourceforge.net/projects/prechacthis'>project site at sourceforge.net</a></td>
  </tr>
  <tr>
   <td class='lable'>&nbsp;</td>
   <td class='input'><a href='http://en.wikibooks.org/wiki/Symmetric_Passing_Patterns'>learn about symmetric passing</a></td>
  </tr>
 </table>
 $hidden_debug
</form>
";


function correctPasses($passes){
	if (!is_numeric($passes)){
		return "_"; //pass to prolog as unspecified
	}
	return $passes;
}


function doku($doku_flag){
if ($doku_flag){
$doku["head"]     = "   <td class='head'>Meaning</td>\n";
$doku["persons"]  = "   <td class='sample'>2 persons</td>\n";
$doku["objects"]  = "   <td class='sample'>4 or 5 objects</td>\n";
$doku["length"]   = "   <td class='sample'>period length 4, 5 or 6.</td>\n";
$doku["max"]      = "   <td class='sample'>highest throw is a 4</td>\n";
$doku["multiplex"]= "   <td class='sample'>no multiplexes</td>\n";
$doku["exclude"]  = "   <td class='sample'>exclude patterns that contain<br>a 0, a 1 1 or a 2 2</td>\n";
$doku["contain"]  = "   <td class='sample'>find patterns that contain<br>3 3p or 3 4p or 3 followed by a pass.</td>\n";
$doku["clubdoes"] = "   <td class='sample'>a club does a 1 or a 2 followed by a 1p or a 1.5p<br>or the other way round.</td>\n";
$doku["react"]    = "   <td class='sample'>throw a 3 underneath an arriving 2p<br>or before catching a 4p throw a 4<br>or react with a 1.5p to a 3.</td>\n";
$doku["results"]  = "   <td class='sample'>don't show more than 42 results.</td>\n";
}else{$doku="";}
return $doku;
}
?>

<?php echo "<div id='path'>" . strstr($_SERVER["SCRIPT_FILENAME"], "/prechacthis/") ."</div>";?>
</body>
</html>


