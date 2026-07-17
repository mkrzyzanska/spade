app_texts <- list(
 title="Example 1: Flinders Petrie's date of birth",
  QoI="<h5>
        <div>
    <b>YEAR</b> of birth of Flinders Petrie 
    </div>
    </h5>",
  evidence_portfolio = "
    <h5>
      <ul>
        <li>Throughout his career Petrie conducted surveys and excavations across a number of archaeological sites in Egypt, such as Giza, Amarna and Abydos.</li>
        <li>By the 1910s, Petrie’s reputation in archaeology had grown considerably, leading him to sell his extensive collection of Egyptian artefacts to University College London, which would later form the core of the Petrie Museum of Egyptian Archaeology.</li>
      <li>In 1923, Petrie was knighted, reflecting his groundbreaking contributions to archaeology.
    </ul>
    </h5>",
    intro_screen= "
    <h5>
      <div>
        <p>Welcome to the first of our exercises designed to introduce you to formal elicitation methods! In this exercise, you’ll learn how to elicit a formal probability distribution for a date based on limited, mostly qualitative information. To help make the method more intuitive, we’ll begin with something more familiar than archaeological dates—a date of birth.</p>

        <p>More specifically, you’ll be asked to estimate the <b>year of birth</b> for Flinders Petrie. In elicitation, the value we want to estimate is called a <b>Quantity of Interest</b> or <b>QoI</b> and needs to be defined in a clear and precise way. For example, note that here we are interested specifically in the <b>year</b> of birth. While our <b>QoI</b> can be easily verified with a quick internet search, for the purpose of the exercise, please rely solely on the information provided here, along with any background knowledge you may already have.</p>

        <p>You may already be familiar with Flinders Petrie, who was instrumental in developing seriation methods and laying the groundwork for using artefacts as dating evidence. As you work through this exercise, feel free to draw on any background knowledge you have about Petrie. To assist you in making judgments about his year of birth, we’ve prepared a short portfolio of key information from his life. Click the button below to reveal it.</p>
      </div>
    </h5>",
   s2="<h5>
        <div>
        The evidence portfolio (to the left) will remain open on the side for easy reference throughout the exercise. It is an integral part of the elicitation process, and later on, you will learn how to prepare your own evidence portfolios for archaeological artefacts. For now, to keep things simple, focus on the information provided and click 'Next' when you are ready to proceed.
    </div>
  </h5>",
 s3="<h5>
        <div>
    You will now be asked to make two quantitative judgments based on the information available about Petrie’s life, as well as any background knowledge you think is relevant (such as, for example, typical human lifespans). The first judgment involves identifying the earliest plausible year of birth for Petrie. To help make this decision, consider which years would be implausible or outright impossible based on the available information. For instance, if Petrie were born in 1800, he would have had to be over 100 years old when he donated artefacts to University College London and was knighted—while not impossible, this would be an exceptionally advanced age. Choose a date such that you would be extremely surprised if Petrie had been born before it, and enter it in the box below. Similarly, identify the latest plausible year of birth and enter it in the corresponding space. Once you are happy with your judgments, click 'Next' to proceed.
    </div>
      </h5>",
s4="<h5>
        <div>
    Now, we move on to the next stage of the elicitation process—where you are going to place 20 tokens on the graph above. You can allocate the tokens by clicking between the vertical lines that divide the graph into a series of time intervals. However, don't try to allocate them all at once! Start by placing 1 token in each interval that you believe has at least a 1 in 20 chance of containing Petrie’s year of birth. To do this, simply click on the interval on the graph where you'd like to place the token. For now, focus on placing the tokens in the bottom row of the graph—click the button below to highlight this area. If you think there is less than a 1 in 20 chance of Petrie’s year of birth falling within an interval, leave it empty. If you want to remove a token, simply click on it again.
    </div>
      </h5>",
s4.2="<h5>
        <div>
    Note that the intervals without any tokens still have a small chance of containing Petrie’s year of birth, but it will be less than 1 in 20. So, don’t worry about leaving them empty. When you are satisfied with your initial token allocation, click 'Next' to continue.
    </div>
      </h5>",

s5="<h5>
        <div>
    Now it is time to allocate the remaining tokens. Have a look at the time intervals which already have tokens allocated and place one on top of each one that you think has at least a 2 in 20 chance of containing Petrie's year of birth. If you have more tokens remaining, do the same with 3 in 20, and then with 4 in 20, etc. You need to allocate all 20 tokens before you can proceed. Once you have done that and are happy with their placement, click next.
 </div>
      </h5>",
s6="<h5>
        <div>
    The next step in elicitation is to fit the curve that will represent the probability distribution, based on the judgements you have made by placing the tokens on the graph. There are a number of different distributions that can be used, but for now we focus on the one that was automatically determined as best-fitting (more on that in the next exercise). Have a look at it on the graph below, as it overlays the tokens you placed to see how well the shape represents your beliefs. Each of the regions coloured in red represents 10% of the probability distribution.


    </div>
      </h5>",

s6.1="<h5>
        <div>
    Visual inspection of the curve is important to check if a selected distribution is a good fit. In actual elicitation and later examples you will be able to choose a different one, if the default does not work well. However, to ensure that the probability distribution accurately reflects your beliefs about the inferred date, it is important to review what it implies and make adjustments if needed by relocating some tokens. This step is crucial, and it's completely normal to need adjustments. In fact, the entire process may need to be repeated several times to ensure that the distribution aligns closely with your expertise. To help verify this, please consider the 5 statements below and evaluate whether they seem reasonable to you:


</div>
      </h5>",
s6.2="<h5>
        <div>
     You can also change the plausible range of values below (although <b> note that this will delete your current allocation and you will have to start from scratch </b>). After you re-allocate all the tokens, click the <b>Update yout judgments button </b> above to update the statements and review them again.
    </div>
      </h5>",
s6.3="<h5>
        <div>
If you disagree with any of these statements, try relocating the tokens to better reflect your beliefs and see how they affect the implications of the token allocation. Click the button below to go back to the token allocation screen.

</div>
      </h5>
",

s6.5="<h5>
        <div>
Once you are happy with the new allocation, click the button below to go back to the previous screen to update your judgements go through the review process again.

</div>
      </h5>
",
s6.4="<h5>
        <div>
Once you are confident that the token allocation accurately reflects your beliefs about Petrie's year of birth, you can download it as an image or a CSV file. The CSV file can be later imported into the elicitation software.
    </div>
      </h5>",
s7="<h5>
        <div>
    Congratulations! You have completed the exercise! The purpose of the exercise was to accurately represent <b> your beliefs </b> about Petrie's year of birth, rather than getting it accurately. However, you may be interested to know, that it was 1853. This date may not necessarily fall in the middle of your estimates—and this is fine! After all you are always going to be uncertain about the quantity of interest, and the whole purpose is to represent this uncertainty in a quantitaive way. However, if this value fell completely outside earliest and latest plausible limits you provide, you may want to re-consider how you made those judgements.  
    </div>
          </h5>"

)

