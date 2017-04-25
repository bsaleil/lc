# Usage: python thisscript.py path/to/csv/file.csv refexec "col1:col2:col3" "name1:name2:name3"
# ex.
# python test.py test.csv exec1 "%exec3:%exec2" "Une barre:Une seconde barre" && evince test.pdf

import sys

CSV_FILE  = sys.argv[1]
REF_COL   = sys.argv[2]
DRAW_COLS = sys.argv[3].split(":")
BAR_NAMES = sys.argv[4].split(":")


FONT_SIZE = 10

COLOR_MAX = 0.1
COLOR_MIN = 0.5

import matplotlib.pyplot as plt; plt.rcdefaults()
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib.ticker import FuncFormatter

class Graph:
    def __init__(self,dataobj):
        self.dataobj = dataobj
        self.fig = None
        self.axes = None

    #private
    def setup_plt(self):
        plt.rcParams['axes.linewidth'] = 0.6
        plt.rcParams['text.usetex'] = True
        font = {'family' : 'serif',
            'weight' : 'normal',
            'size'   : FONT_SIZE}
        plt.rc('font', **font)
        fig, axes = plt.subplots(figsize=(7,2.5))
        # Hide small guide lines of y axis and hide top lines of x axis
        plt.tick_params(axis=u'both', which=u'both',length=0)
        self.fig = fig
        self.axes = axes

    def setup_axes(self,axes,nb_x_els):
        # Show y grid below graph
        axes.yaxis.grid(True)
        axes.set_axisbelow(True)
        # Change grid style to dotted 0.3 lines
        gridlines = axes.get_xgridlines() + axes.get_ygridlines()
        for line in gridlines:
            line.set_linestyle(':')
            line.set_linewidth(0.3)
        # Set x axis limit
        axes.set_xlim([0,nb_x_els])
        # Rotate x labels
        labels = axes.get_xticklabels()
        plt.setp(labels, rotation=45)
        # Add latex '%' to ylabels
        formatter = FuncFormatter(lambda y,pos:str(int(y))+r'$\%$')
        axes.yaxis.set_major_formatter(formatter)

    def draw_legend(self,nb_bars):
        legend = plt.legend(bbox_to_anchor=(0.0, 1.02, 1., .0), loc=3, ncol=nb_bars, mode="expand", borderaxespad=0.,fontsize=FONT_SIZE,fancybox=False)
        # Transparent background
        legend.get_frame().set_facecolor('none')
        # Black 0.6 frame
        legend.get_frame().set_edgecolor('black')
        legend.get_frame().set_linewidth(0.6)

    def export(self,pdfout):
        plt.tight_layout() # Set layout size to fig size
        if pdfout:
            with PdfPages(pdfout) as pdf:
                pdf.savefig(self.fig,bbox_inches='tight')
        else:
            plt.show()

    def prepare_data(self):
        xlabels = self.dataobj.rowLabels
        data = []
        for label in self.dataobj.colLabels:
            col_data = []
            colidx = self.dataobj.colLabels.index(label)
            for line in self.dataobj.data:
                col_data.append(line[colidx])
            data.append(col_data)
        #data.append([0]*len(data[0]))
        return xlabels, data

    def draw_bars(self,xlabels,data,bar_labels):
        # Compute colors
        if (len(data) == 1):
            color_step = COLOR_MAX - COLOR_MIN
        else:
            color_step = (COLOR_MAX - COLOR_MIN) / (len(data)-1)
        colors = np.arange(COLOR_MIN,COLOR_MAX+color_step,color_step)
        colors = list(map(str, colors))
        #
        y_pos = np.arange(len(xlabels))
        bar_width = 1.0/(len(data)+1)
        margin = 0
        i = 0
        for d in data:
            plt.bar(y_pos+margin,d,width=bar_width, align='edge', alpha=1.0,label=bar_labels[i],color=colors[i])
            margin += bar_width
            i += 1
        plt.xticks(y_pos+((1.0-bar_width)/2.0), xlabels)

    def draw(self,bar_labels):
        xlabels, data = self.prepare_data()
        assert len(bar_labels) == len(data)
        # Global setup
        self.setup_plt();
        # Draw bars
        self.draw_bars(xlabels,data,bar_labels)
        # Draw legend
        self.draw_legend(len(data));
        # Setup axes & grid
        self.setup_axes(self.axes,len(xlabels));
        # Export to pdf or show
        self.export("./test.pdf");

class Data:
    def __init__(self,rowLabels,colLabels,data):
        self.rowLabels = rowLabels
        self.colLabels = colLabels
        self.data = data

    def insert_col(self,colname,coldata):
        # Check length
        self.colLabels.append(colname);
        i = 0
        for data in self.data:
            data.append(coldata[i])
            i+=1

    def compute_relative_col(self,colname,refname):
        colidx = self.colLabels.index(colname)
        refidx = self.colLabels.index(refname)
        result = []
        for data in self.data:
            ref = data[refidx]
            val = data[colidx]
            result.append(val*100.0/ref)
        return result

    def extract_cols(self,colLabels):
        data = []
        for line in self.data:
            line_data = []
            for colLabel in colLabels:
                colidx = self.colLabels.index(colLabel)
                line_data.append(line[colidx])
            data.append(line_data)
        return Data(self.rowLabels,colLabels,data)

    @staticmethod
    def from_csv_file(path):
        with open(CSV_FILE, 'r') as file:
            lines = file.readlines();
            colLabels = lines[0].replace("\n","").split(';')[1:]
            rowLabels = list(map(lambda x:x.split(";")[0],lines))[1:]

            data = []
            for line in lines[1:]:
                strs = line.replace("\n","").split(";")[1:]
                nums = list(map(float,strs))
                data.append(nums)
            return Data(rowLabels,colLabels,data)

dataobj = Data.from_csv_file(CSV_FILE);
labels = list(dataobj.colLabels) # Copy label list
for colLabel in labels:
    if colLabel != REF_COL:
        r = dataobj.compute_relative_col(colLabel,REF_COL)
        dataobj.insert_col("%"+colLabel,r)

do = dataobj.extract_cols(DRAW_COLS);

graph = Graph(do)
graph.draw(BAR_NAMES)
