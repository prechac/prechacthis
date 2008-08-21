<?php
if (isset($_REQUEST["pattern"])) {
    header("Content-type: application/force-download");
    header("Content-Transfer-Encoding: Binary");
    header("Content-disposition: attachment; filename=\"".$_REQUEST["pattern"].".pass\"");
    echo("test");
}
?>