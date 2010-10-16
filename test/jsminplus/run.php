<?php

include "jsminplus.php";

//$minified = JSMinPlus::minify($script [, $filename])

//$script="{x=1;y=1;}";
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

echo (JSMinPlus::minify($script, "foo"))
?>
