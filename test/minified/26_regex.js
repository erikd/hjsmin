function verifyZipCode(zip){zip=new String(zip);pattern=/[0-9]{5}([- ]?[0-9]{4})?/;if(pattern.test(zip)){return zip.match(pattern)[0]}else return""};a=verifyZipCode(95060)
