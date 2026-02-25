# Collecting and Analyzing Rhizodeposits: Reviewing and Optimizing Tool (CARROT)

**Licence:** [CeCILL-C](http://www.cecill.info/licences/Licence_CeCILL-C_V1-en.html)  

### Aims & purposes

<i>CARROT</i> is a decision support tool aiming to guide a user towards the protocol of rhizodeposits collection and analysis best suited for his/her own objectives and constraints. The tool enables to move along a complex decision tree in order to identify options for successive protocol steps that are either recommended, alternative or incompatible with the set of choices progressively selected by the user. CARROT does not intend to create a complete, operational protocol of rhizodeposit collection & analysis, but rather aims to orientate the user towards the best methods by giving him/her synthetic information.

### How to start <i>CARROT</i>

<i>CARROT</i> has been written as an R Shiny application. The package available on https://github.com/frees86/carrot contains in particular:
- the R file "app.R" containing the original code,
- the folder "source" containing the source files (e.g. the decision tree and the Supporting Information tables), 
- the folder "docs" that allows to generate the corresponding GitHub page,<br/>
- the PDF file "CARROT User manual.pdf".<br/>

The program <i>CARROT</i> can be either:
- launched online without any installation by accessing the corresponding GitHub Page (https://frees86.github.io/carrot)
- used as an R program by executing the file ‘app.R’ in an R console - <i>note that the file 'app.R' needs to be located in the same folder as the folder 'source' containing the supporting tables. </i>

### How to use <i>CARROT</i>

After launching the program, the first step is to select one of the five groups of instructions from which to start:
1.	Scientific questions & objectives
2.	Growth conditions
3.	Sampling method
4.	Sample treatment
5.	Sample analysis<br/>

<i> Note: It is recommended to start by “Scientific questions & objectives” and to follow the logical order of these successive groups of instructions. However, the user can choose to go through these groups of instructions in any order, knowing that this order has consequences on the evaluation of the compatibility of successive options to one another. </i><br/>
<br/>
The next step is to select one of the possible options corresponding to the first instruction. For this instruction and all subsequent ones, a Supporting Information table is displayed on the right panel and aims to provide relevant, synthetic information to support the decision. Additional information is also usually provided in the companion scientific article.<br/>
<br/>
Once a first option has been selected and the instruction for a new protocol step is considered, a compatibility test is activated, and automatically labels each option as “Recommended”, “Possible” or “Incompatible”, based on previous choices. The reason for which a given option is deemed incompatible can be further explored by ticking the box “Show details about incompatible options” displayed above the instruction.<br/>
<br/>
<i> Note: if all possible options of a given instruction are deemed incompatible with previous choices, the instruction will be skipped and the program will automatically display the next instruction without notice. </i><br/>
<br/>
<i> Note: Depending on your progression within a given group of instructions, you can click on “Go back” to move back to the previous instruction of the current group of instruction, or click on “Skip this” to move directly to the next instruction without deciding - this may be used in particular to avoid a step to influence the compatibility suggested for the options of the next instructions The decision to skip instruction will appear in the final results. </i>
<br/>
<br/>
When all possible instructions within one group of instructions have been covered, a new group of instructions can be selected, and the selection procedure can continue. Note that a group of instructions can be revisited later one. In such case, a warning is displayed to confirm the choice, and, when confirmed, the previous choices related to this group of instruction will be overwritten.<br/>

At any moment within the protocol construction, the user can access and download the resulting protocol corresponding to the selected choices by moving to the tab “Updated protocol”. There, the user can:
- download the protocol as a CSV file or Excel file summarizing the instruction and corresponding choice in each group, as well as detailing the compatibility test for each option, 
- download a text file summarizing the main steps of the protocol.

Once all possible options within each group of instructions, the user can leave or continue the program. Two options are available:
- RE-START: All previous answers will be erased and the program will reinitialize at the starting page.
- RESUME: The user can reconsider one of the previously visited groups of instructions. If so, the answers corresponding to this group of instructions will be erased, but other previous answers will be kept and will influence the compatibility of options for the revisited group.

### Reference

The decision support tool <i>CARROT</i> is associated to a companion scientific article, currently under review:

<i>Frédéric Rees, Virginie Lauvergeat, Gabin Piton, Aude Tixier, Sylvain Chéreau, Sylvie Dinant, Barbara Pawlak, François Perreau, Emmanuelle Personeni, Jean-Bernard Pouvreau, Anouk Zancarini, Jean-Benoît Peltier, and Agnès Attard (2026). Unlocking research on rhizodeposition: a step-by-step guide for producing, sampling and analyzing rhizodeposits.</i>

### Credits

This works originates from discussions held within the French network <i>RhizosPHARE</i> and the previous project <i>PHARE</i> (2021-2022) funded by INRAE.<br/>

- <b>Conception & programing:</b> Frédéric Rees, Sylvain Chéreau
- <b>Writing, editing & testing:</b> Frédéric Rees, Virginie Lauvergeat, Jean-Benoît Peltier, Gabin Piton, Aude Tixier, Sylvain Chéreau, Sylvie Dinant, Barbara Pawlak, François Perreau, Emmanuelle Personeni, Jean-Bernard Pouvreau, Alexandre de Saint Germain, Anouk Zancarini, Agnès Attard
