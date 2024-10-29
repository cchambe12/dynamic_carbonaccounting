"""
FVS utility functions
"""

# Core imports
from dataclasses import dataclass
import os
import pathlib
from string import Template
import subprocess


@dataclass
class KeywordFile:
    """Class for keyword file template injection
    """

    template_name: str

    def __post_init__(self):
        """Read the template from disk after initialization

        Raises:
            FileNotFoundError: Exception if keyword template doesn't exist
        """

        template_dir = pathlib.Path(__file__).parent / "templates"
        kw_path = f'{template_dir}/{self.template_name}.txt'

        if not os.path.exists(kw_path):
            raise FileNotFoundError

        with open(kw_path, 'r') as f:
            self.kw_template = Template(f.read())

    def publish(self, plot_id: str, state: str, year: int) -> str:
        """Publish the keyword file by injecting values into the template

        Args:
            plot_id (str): Plot ID (PLT_CN in FIA)
            state (str): State alpha code (2 letters, case insensitive)
            year (int): Year to harvest at

        Returns:
            str: Keyword file
        """

        # Inject values in template
        kw_file = self.kw_template.substitute(
            plot_id=plot_id, state=state.upper(),
            year=year)

        return kw_file


def run(kwfile: str) -> int:
    """Run FVS simulation by supplying a keyword file

    Args:
        kwfile (str): Keyword file

    Returns:
        int: FVS status code
    """

    # Set the path to the FVS lake states variant executable
    #exepath = './open-fvs2/trunk/bin/FVSls.exe'
    exepath = '/Users/c.chamberlain/Documents/git/ForestVegetationSimulator-Interface-main/bin/FVSsn'

    # Write the keyword file to disk
    with open('./input/tmp.key', 'w') as f:
        f.write(kwfile)

    # Run FVS and fetch stop code
    stdout = subprocess.run(
        [exepath, f'--keywordfile=./input/tmp.key'], stdout=subprocess.DEVNULL,
        stderr=subprocess.STDOUT)

    # Return the FVS status code
    return stdout.returncode
