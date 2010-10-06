// From http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Statements:throw

function UserException(message) {
   this.message = message;
   this.name = "UserException";
}
function getMonthName(mo) {
   mo = mo-1; // Adjust month number for array index (1=Jan, 12=Dec)
   var months = new Array("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
      "Aug", "Sep", "Oct", "Nov", "Dec");
   if (months[mo] != null) {
      return months[mo];
   } else {
      myUserException = new UserException("InvalidMonthNo");
      throw myUserException;
   }
}

try {
   // statements to try
   monthName = getMonthName(14);
} catch (e) {
   monthName = "unknown";
   x = e.name
}

monthName == "unknown" && x == "UserException"