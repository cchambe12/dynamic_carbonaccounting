"""
Main program for pumping through the simulations
"""

# Internal imports
from fia import OAK, MBB
from fvs import KeywordFile, run
from tqdm import tqdm


# ------------------------------------------------------------------------------
# OAK SIMULATIONS
# ------------------------------------------------------------------------------

# Build list of mbb statewide FIA queries
oak_states = [OAK(state='al'), OAK(state='ky'),
              OAK(state='nc'), OAK(state='sc'), OAK(state='tn'),OAK(state='va')]

# Build list of mbb scenario keyword files to simulate
oak_kws = [KeywordFile('oak-grow'),
           KeywordFile('oak-bau')]

# Run FVS on each plot for each scenario in each state
for state in tqdm(oak_states, desc='states', position=0, leave=False):
    for kw in tqdm(oak_kws, desc='keywords', position=1, leave=False):
        for plot in tqdm(state.plots, desc='plots', position=2, leave=False):
            rtn = run(kw.publish(plot, state.state))


# ------------------------------------------------------------------------------
# MBB SIMULATIONS
# ------------------------------------------------------------------------------

# Build list of mbb statewide FIA queries
mbb_states = [MBB(state='al'), MBB(state='ky'),
              MBB(state='nc'), MBB(state='sc'), MBB(state='tn'),MBB(state='va')]

# Build list of mbb scenario keyword files to simulate
mbb_kws = [KeywordFile('mbb-grow'), KeywordFile('mbb-bau')]

# Run FVS on each plot for each scenario in each state
for state in tqdm(mbb_states, desc='states', position=0, leave=False):
    for kw in tqdm(mbb_kws, desc='keywords', position=1, leave=False):
        for plot in tqdm(state.plots, desc='plots', position=2, leave=False):
            rtn = run(kw.publish(plot, state.state))
            
            