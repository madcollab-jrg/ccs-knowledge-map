console.log("check");
// shinyjs.disableButton = function () {
//   $("#run_report").prop("disabled", true);
// };

// shinyjs.enableButton = function () {
//   $("#run_report").prop("disabled", false);
// };

// // Disable the button initially
// shinyjs.disableButton();

// window.onload = () => {
//   // Disable the button initially
//   document.getElementById("run_report").disabled = true;

//   document.addEventListener("change", () => {
//     var surveyValue = $("#survey").val();
//     console.log({ surveyValue });
//     if (surveyValue !== "") {
//       document.getElementById("run_report").disabled = false;
//     } else {
//       document.getElementById("run_report").disabled = true;
//     }
//   });
// };

$(document).ready(function () {
  // Disable the button initially
  $("#run_report").prop("disabled", true);

  // Monitor changes in the survey dropdown
  $(document).on("change", "#survey", function () {
    var surveyValue = $(this).val();
    if (surveyValue !== "") {
      $("#run_report").prop("disabled", false);
    } else {
      $("#run_report").prop("disabled", true);
    }
  });
});

// Enable/disable the button based on input selection
// document.addEventListener("input", function () {
//   var surveyValue = $("#survey").val();
//   var censusLevelValue = $("#census_level").val();
//   var demographicValue = $("#demographic").val();

//   if (
//     surveyValue !== "" &&
//     censusLevelValue !== "" &&
//     demographicValue !== ""
//   ) {
//     shinyjs.enableButton();
//   } else {
//     shinyjs.disableButton();
//   }
// });

// Hide all census levels except Zipcode for specific surveys
// $(document).on("change", "#survey", function () {
//   // var surveyValue = $("#survey").val();
//   var censusDropdown = $("#census_level")
//     .siblings(".shiny-input-select")
//     .find(".selectize-dropdown-content");

//   // console.log(surveyValue);

//   console.log(censusDropdown);

//   var surveyValue = $("#survey").val();
//   var selectedText = $("#survey").find("option:selected").text();
//   console.log(selectedText);

//   if (selectedText === "Tree Knowledge" || selectedText === "Carbon Concerns") {
//     // Hide all options except "Zipcode"
//     censusDropdown.find("option").each(function () {
//       if ($(this).val() !== "Zipcode") {
//         $(this).hide();
//       } else {
//         $(this).show();
//       }
//     });
//     // Set the census dropdown to "Zipcode" and trigger change
//     censusDropdown.val("Zipcode").trigger("change");
//   } else {
//     // Show all options
//     censusDropdown.find("option").show();
//     // Reset the dropdown selection
//     censusDropdown.val("").trigger("change");
//   }
// });
