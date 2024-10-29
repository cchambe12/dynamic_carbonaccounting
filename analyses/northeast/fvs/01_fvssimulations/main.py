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
oak_states = [OAK(state='ma'), OAK(state='me'), OAK(state='nh'),
              OAK(state='ny'), OAK(state='vt')]

# Build list of mbb scenario keyword files to simulate
oak_kws = [KeywordFile('oak-grow'), KeywordFile('oak-bau'),
           KeywordFile('oak-cut25')]

# Run FVS on each plot for each scenario in each state
for state in tqdm(oak_states, desc='states', position=0, leave=False):
    for kw in tqdm(oak_kws, desc='keywords', position=1, leave=False):
        for plot in tqdm(state.plots, desc='plots', position=2, leave=False):
            rtn = run(kw.publish(plot, state.state, 10))


# ------------------------------------------------------------------------------
# MBB SIMULATIONS
# ------------------------------------------------------------------------------

# Build list of mbb statewide FIA queries
mbb_states = [MBB(state='ma'), MBB(state='me'), MBB(state='nh'),
                MBB(state='ny'), MBB(state='vt')]

# Build list of mbb scenario keyword files to simulate
mbb_kws = [KeywordFile('mbb-grow'), KeywordFile('mbb-bau'),
           KeywordFile('mbb-cut25')]

# Run FVS on each plot for each scenario in each state
for state in tqdm(mbb_states, desc='states', position=0, leave=False):
    for kw in tqdm(mbb_kws, desc='keywords', position=1, leave=False):
        for plot in tqdm(state.plots, desc='plots', position=2, leave=False):
            rtn = run(kw.publish(plot, state.state, 10))
