# update_prediction_and_plot_r

    Code
      update_predictions_and_plot_r(prediction_custom_scenario = "testing",
        model_outputs = load_model_object("wf"), scenario_name = "incorrect_name",
        performance_metrics = "Proportion of attended GP appointments (over 4 weeks wait time)",
        r = list(ics_cd = ics_code, ics_data = all_ics_data, scenario_data = scenario_data))
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "last_known", "percent", "linear", "custom"

---

    Code
      custom_test <- update_predictions_and_plot_r(prediction_custom_scenario = "testing",
        model_outputs = load_model_object("wf"), scenario_name = scenario,
        performance_metrics = "Proportion of attended GP appointments (over 4 weeks wait time)",
        r = list(ics_cd = ics_code, ics_data = all_ics_data, scenario_data = scenario_data,
          predictions = predictions))
    Condition
      Error in `session$sendModal()`:
      ! attempt to apply non-function

