# Nutrition-programming
Optimizing the allocation of supercereal food commodities to flood-affected Wards in Nepal

Annual flooding in Nepal's Terai region exacerbates an already alarming child malnutrition situation. The World Food Programme (WFP) crafts its response to floods based on an assessment of affected areas and persons (extent, severity and drivers), existing responses, intra-UN programming coordination, and available resources. The standard emergency nutrition intervention in this case is known as a Blanket Supplementary Feeding Programme (BSFP). Through BSFP, WFP transfers vitamin and mineral-fortified 'supercereal' to all pregnant and lactating women, and children between the ages of 6-59 months, within a given geographical area. Household level targeting is not done for BSFP; all eligible persons within the geographical areas receive the assistance. in July 2019, Nepal's Terai region suffered widespread flooding, with at least 6 Districts severely affected.

The nature of BSFP distribution poses a unique challenge in the context of flooding however, as unlike emergencies which tend to affect areas uniformly (such as drought), flooding can be highly localized. Given this, a blanket food distribution is likely to include people who are not in need of the food--in this case, pregnant and lactating women, and children 6-59 months who were not directly affected by flooding (for example, whose household food stores were not destroyed, etc.). Given limited resources, WFP's assistance should maximize the number of eligible people who were affected, who receive assistance.

The goal of this analysis is to achieve that optimization across Wards (the lowest administrative unit in Nepal. Wards are nested into Palikas, nested into Districts. A Ward may have a population of between 2,000-8,000 inhabitants) in the 6 most flood-affected Districts. This analysis treats the problem as a linear programming constrained optimization problem, specifically as a knapsack problem in which the inclusion of a unit (here a Ward) contributes to an overal objective function, but at a cost (the constraint) of resources (in metric tonnes of supercereal). This analysis optimizes the combination of Wards in which to intervene at different levels of supercereal metric tonnes (representing different scenarios/possible resourcing levels).

The percentage of population affected (pop_affect_perc) is a measure of the estimated percentage of people affected by flooding in each ward. From this measure (obtained via overlaying settlement-level population estimates with Sentinel-1 inundation mapping) we obtain an estimate of the number of affected pregnant and lactating women and of children under 5 in each Ward. This parameter becomes the objective function to maximize in the models.

The most efficient allocation across wards is run considering different available amounts (metric tonnes) to distribute (itself a function of the organization's resourcing, though this analysis could be used to lobby donors for funding). These amounts range from 300 MTs to 1000 MTs.Finally three different visuals are designed in order to help management and programme officers to allocate resources across different wards.

In the future this type of analysis could shape both the immediate Ward-level prioritization of assistance and be used as a tool to lobby donors for resources and to guide intra-UN response coordination efforts. Ultimately many factors will weigh into the decision on where to allocate food assistance, and this type of analysis will be but one of those.
