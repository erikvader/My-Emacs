# This is a sample commands.py.  You can add your own commands here.
#
# Please refer to commands_full.py for all the default commands and a complete
# documentation.  Do NOT add them all here, or you may end up with defunct
# commands when upgrading ranger.

# You always need to import ranger.api.commands here to get the Command class:
from ranger.api.commands import *

# A simple command for demonstration purposes follows.
#------------------------------------------------------------------------------

# You can import any python module as needed.
import os

# Any class that is a subclass of "Command" will be integrated into ranger as a
# command.  Try typing ":my_edit<ENTER>" in ranger!
class my_edit(Command):
    # The so-called doc-string of the class will be visible in the built-in
    # help that is accessible by typing "?c" inside ranger.
    """:my_edit <filename>

    A sample command for demonstration purposes that opens a file in an editor.
    """

    # The execute method is called when you run this command in ranger.
    def execute(self):
        # self.arg(1) is the first (space-separated) argument to the function.
        # This way you can write ":my_edit somefilename<ENTER>".
        if self.arg(1):
            # self.rest(1) contains self.arg(1) and everything that follows
            target_filename = self.rest(1)
        else:
            # self.fm is a ranger.core.filemanager.FileManager object and gives
            # you access to internals of ranger.
            # self.fm.thisfile is a ranger.container.file.File object and is a
            # reference to the currently selected file.
            target_filename = self.fm.thisfile.path

        # This is a generic function to print text in ranger.  
        self.fm.notify("Let's edit the file " + target_filename + "!")

        # Using bad=True in fm.notify allows you to print error messages:
        if not os.path.exists(target_filename):
            self.fm.notify("The given file does not exist!", bad=True)
            return

        # This executes a function from ranger.core.acitons, a module with a
        # variety of subroutines that can help you construct commands.
        # Check out the source, or run "pydoc ranger.core.actions" for a list.
        self.fm.edit_file(target_filename)

    # The tab method is called when you press tab, and should return a list of
    # suggestions that the user will tab through.
    def tab(self):
        # This is a generic tab-completion function that iterates through the
        # content of the current directory.
        return self._tab_directory_content()


class toggle_flat(Command):
    """
    :toggle_flat

    Flattens or unflattens the directory view.
    """

    def execute(self):
        if self.fm.thisdir.flat == 0:
            self.fm.thisdir.unload()
            self.fm.thisdir.flat = -1
            self.fm.thisdir.load_content()
        else:
            self.fm.thisdir.unload()
            self.fm.thisdir.flat = 0
            self.fm.thisdir.load_content()

class quick_nav(Command):
    """
    :quick_nav [down|up|_anything_else_]

    Move prefix arg lines up or down and enter the new target if it is
    a directory. If the first argument is neither down or up, prefix
    arg is treated as the absolute position to go to.
    """
    def execute(self):
        # get prefix arg
        if self.quantifier:
            amount = self.quantifier
        else:
            amount = 1

        # move in direction according to first argument
        if self.arg(1) == "down":
            self.fm.move(down=amount)
        elif self.arg(1) == "up":
            self.fm.move(up=amount)
        else:
            self.fm.move(to=amount)

        # enter the thing if it is a directory
        if self.fm.thisfile.is_directory:
            self.fm.move(right=1)
        else:
            self.fm.notify("not a directory, can't enter it!")

class toggle_super_zoom(Command):
    """
    :toggle_super_zoom

    Toggles whether the middle current section should take up the
    whole window.
    """
    def execute(self):
        if hasattr(self.fm, "is_superzoomed") and self.fm.is_superzoomed == True:
            self.fm.is_superzoomed = False
            self.fm.settings.column_ratios = [1,3,4]
            # self.fm.settings.collapse_preview = False
            self.fm.settings.preview_files = True
            self.fm.settings.preview_directories = True
        else:
            self.fm.is_superzoomed = True
            self.fm.settings.column_ratios = [100000,1]
            # self.fm.settings.collapse_preview = False
            self.fm.settings.preview_files = False
            self.fm.settings.preview_directories = False
