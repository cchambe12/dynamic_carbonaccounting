"""
Main program for pumping through the simulations
"""

# Internal imports
from fia import OAK
from fvs import KeywordFile, run
from tqdm import tqdm


# ------------------------------------------------------------------------------
# OAK SIMULATIONS
# ------------------------------------------------------------------------------

# Build list of mbb statewide FIA queries
oak_states = [OAK(state='mn'), OAK(state='wi'), OAK(state='mi'),
              OAK(state='il'), OAK(state='in'), OAK(state='ia'),OAK(state='oh')]

# Build list of mbb scenario keyword files to simulate
oak_kws = [KeywordFile('oak-grow'), KeywordFile('oak-bau'),
           KeywordFile('oak-gmf25')]

# Run FVS on each plot for each scenario in each state
for state in tqdm(oak_states, desc='states', position=0, leave=False):
    for kw in tqdm(oak_kws, desc='keywords', position=1, leave=False):
        for plot in tqdm(state.plots, desc='plots', position=2, leave=False):
            rtn = run(kw.publish(plot, state.state, 10))