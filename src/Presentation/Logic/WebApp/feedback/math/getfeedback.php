<?php
 include_once("../common/post-get.php");
/*
 * Wordt aangeroepen wanneer de gebruiker een nieuwe
 * set van formules heeft ingevoerd en op
 * Submit klikt.
 * We sturen de gegevens door
 * en geven het resultaat terug
 */
$result = post_it($_POST, "http://localhost:8000/math/feedback/");
printf($result);
?>