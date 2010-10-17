#!/usr/bin/php -q
<?php

include "jsminplus.php";

//$minified = JSMinPlus::minify($script [, $filename])

//$script="{x=1;y=1;}";
/*
$script = <<<EOD

// -- Animal --
function Animal(name)
{
  if (!name)
    throw new Error('Must specify an animal name')
  this.name = name
}

Animal.prototype.toString = function()
{
  return this.name
}

o = new Animal("bob")

o.toString() == "bob";

EOD;
*/
/*
echo "Test Arguments:\n";
echo $_SERVER["argc"]."\n";
echo $_SERVER["argv"][0]."\n";
echo $_SERVER["argv"][1]."\n";
*/

//$filename = '../0_helloworld.js';
$filename = $_SERVER["argv"][1];

$script = file_get_contents($filename);

//echo $script

echo (JSMinPlus::minify($script, "foo"));
echo "\n";

?>
