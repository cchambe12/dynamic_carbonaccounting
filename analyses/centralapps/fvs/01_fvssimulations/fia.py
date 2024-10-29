"""
FIA utility functions
"""

# Internal imports
from dataclasses import dataclass, field
import os
import sqlite3
from typing import NamedTuple

# External imports
import pandas as pd


class FTGRange(NamedTuple):
    """Typing class for forest type group range values
    """
    min: int = 0
    max: int = 1000


class InvYearRange(NamedTuple):
    """Typing class for inventory year range values
    """
    min: int = 2014
    max: int = 2019


@dataclass
class PlotCollection:
    """Data container for a collection of FIA plots and associated metadata

    Raises:
        FileNotFoundError: Exception when the specified state database does not exist
    """

    state: str
    ftg_range: FTGRange
    inv_range: InvYearRange = InvYearRange()
    plots: list[str] = field(default_factory=list)
    ba_minimum: int = 50
    #ecoregion = "M221"

    def __post_init__(self):
        """Automatically query and filter FIA plot after initialization
        """

        # Connect to the FIA database if exists
        db_path = f'./db/FIADB_{self.state.upper()}.db'
        if os.path.exists(db_path):
            db = sqlite3.connect(db_path)
        else:
            raise FileNotFoundError

        # Configure the query string and query the PLOT table
        qstr = f"SELECT CN FROM PLOT WHERE \
                DESIGNCD = 1 \
                AND INVYR >= {self.inv_range.min} AND INVYR <= {self.inv_range.max};"# \
                #AND SUBSTR(REPLACE(ECOSUBCD, ' ', ''), 1, 3) = '{self.ecoregion}';"
        plots = pd.read_sql_query(qstr, db)

        # Configure the query string and query the CONDITION table
        qstr = f"SELECT PLT_CN, FORTYPCD FROM COND WHERE \
                FORTYPCD >= {self.ftg_range.min} AND FORTYPCD <= {self.ftg_range.max} \
                AND BALIVE >= {self.ba_minimum} \
                AND OWNGRPCD >= 40 \
                AND COND_STATUS_CD = 1 \
                AND CONDPROP_UNADJ = 1;"
        conds = pd.read_sql_query(qstr, db)

        # Inner join the plot and condition dataframes on plt cn
        plot_cond = plots.merge(
            conds, how='inner', left_on='CN', right_on='PLT_CN')

        # Drop the duplicate cn column and return
        self.plots = plot_cond['CN'].to_list()


@dataclass
class OAK (PlotCollection):
    """Plot collection sub class for oak/hickory forest type
    """

    ftg_range: FTGRange = FTGRange(min=500, max=599)
    ba_minimum: int = 50

@dataclass
class MBB (PlotCollection):
    """Plot collection sub class for maple/beech/birch forest type
    """

    ftg_range: FTGRange = FTGRange(min=800, max=899)
    ba_minimum: int = 50


