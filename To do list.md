# Priorities:

**1. Re-factoring:**  

  *Context:*    
  Currently, both app and tutorial contain both UI element and the server function. Ideally, they should use different UI element, but server function should be largely the same, and any differences should be handled with the variable set to the UI type.
  
  *Purpose:*   
  
  - Eliminate repeated code, and therefore ensure that any debugging will only need to be repeated once, rather than multiple times for each UI  
  - Enable easier development of extra tutorials.

**2. Teaching material tutorials development**

*Context*:

There is only one tutorial which introduces basic functionality, but it has only been tested with the limited audience. It also does not exhaustively cover the entire elicitation process.

*Tasks*  

- Test and modify tutotial_1 based on the feedback from the larger audience
- Develop a suite of tutorials that exhaustively covers both the elicitation process and elicitation app functionality (including e.g. inputting metadata).

## Will need to be addressed at some point:

**4. Help section **

  Help tab explaining how the app works in detail should be added as one of the tabs, like originally in SHELF.

**3. Debugging and testing - saving functionality** 

  There has been quite a lot of issues with loading data functionality related to order of re-activity. These have mostly been resolved, but every now and then something weird happens and loading the data breaks some element of the app. These edge cases will need to be identified and testes.
  
**4. Hard limits to probability distributions**

  There are likely to be situations, where the limits provided by the experts should be considered 'hard limits', which are not alwyas covered by available ditribution. Therefore the app should provide a functionality to cut-off the distribution and the dates provided, and recalculate all probabilities. On UI side it could be just a hard-limits tick-box for start and end dates. On the backedend it will have implications across the app. 

## Nice to have:

**5. Automatic bin-widths**

  The algorithm that currently determines automatic bin widths is quite crude and often makes suggestions that are sub-optimal for the ranges provided. Ideally would be refined, so the experts don't need customization bins too often.
  
**6. Automatic QoI**

  Currently the algorithm automatically populates the QoI field from metadata in a predeifiend way which is not always the best - ideally there should be a way for experts to create a customization template that defines static text and text taken from metadata fields.

**7. Distributions comparision** 

  It would be useful to develop a comparison view, where multiple distibution fits can be overlaid on the same graph, or compared side by side, together with the feedback.
  
**8. Multiple artefact dates view**

  It would be useful to develop a functionality that will allow loading dates from multiple artefacts and displaying them in a similiar way to radiocarbon dates.

**9. Clean up**

*Tasks*:

- Get rid of redundant code (especially in saving funcionality)
- Double-check the number of reported significant figure everywhere for dates and probabilities, and ensute probability is never rounded up to 0, for really small probabilities in feedback reporting.
- Add full documentation (e.g. to the functions, for potential cran publication)

**10. Advanced graph settings**

Allow to fully customize the graphs, and dimensions from which they are saved from the UI.

**11. Interoperability with other packages**

It would be useful to have the new elicited dates easy to integrated with radiocabon dates, at least for plotting and possibly for the analysis, using functions and tools already available in packages like rcarbon. See CalDatesTransform.R function for an example that puts elicited date in the format where they can be joined with rcarbon dates.

**12. Splines**

  
 
