from __future__ import print_function
import re
import numpy as np
import os
import sys
from astropy.io import ascii
import glob
from time import sleep
import h5py
import pandas as pd

class readTable:
    '''
    A parent class to be wrapped by other class, in order to read in such as mesa history file.
    These files have very similar structures, typically header+table.

    '''
    def __init__(self, filepath, verbose=True):
        '''
        Args:
            filepath: the path of .history file.
            verbose: whether print info to device. default: True.

        Attributes:
            header: a dictionary containing history file header
            track: a structure numpy array containing the evol track
            colnames: a tuple containing column names

        '''  
        self.filepath = filepath
        if verbose: 
            print('Processing :', self.filepath)
        return
    
    def readFile(self, filepath, headerNameLine=1, headerDataLine=2, tableHeaderLine=6):
        '''
        Reads in a file.
        '''

        with open(self.filepath) as f:
            content = [line.split() for line in f]
        header = {content[headerNameLine-1][i]:content[headerDataLine-1][i] for i in range(len(content[headerNameLine-1]))}
        table = np.genfromtxt(self.filepath, skip_header=tableHeaderLine-1, names=True)
        colnames = table.dtype.names

        return header, table, colnames



class history(readTable):
    '''

    A class to read mesa history files, store the data within, and offer useful routines (?).

    '''
    
    def __init__(self, filepath, verbose=True, ifReadProfileIndex=False):
        '''
        Args:
            filepath: the path of .history file.
            verbose: whether print info to device. default: True.

        Attributes:
            header: a dictionary containing history file header
            track: a structure numpy array containing the evol track
            colnames: a tuple containing column names
            profileIndex: a structured array containing the map between model_number and profile_number

        '''
        super().__init__(filepath, verbose)
        self.header, self.track, self.colnames = self.readFile(filepath, headerNameLine=2, headerDataLine=3, tableHeaderLine=6)
        
        if ifReadProfileIndex:
            self.profileIndex = self.read_profile_index()
        return
    
    def read_profile_index(self):
        '''
        Reads in the profile.index file
        '''
        filepath = self.filepath.split('.history')[0] + 'profile.index'
        profileIndex = np.genfromtxt(filepath, skip_header=1, names=('model_number', 'priority', 'profile_number'))
        return profileIndex


def set_mesa_inlist(index, inlist_path, outlist_path, 
                    mass, Xinit, Yinit, Zinit, amlt, 
                    fov_shell, fov0_shell, 
                    fov_core, fov0_core, 
                    ifsetfinalmodel):
    # reads in the template inlist and writes out a new inlist with the 
    # parameters set appropriately
    
    try:
        inlist = open(inlist_path,'r')
        outlist = open(outlist_path,'w')
    except:
        sleep(2.0)
        inlist = open(inlist_path,'r')
        sleep(2.0)
        outlist = open(outlist_path,'w')


    for line in inlist.read().split('\n'):

        first = line.split()
        if len(first)>0:
            if first[0] != '!':

                if re.search('initial\_mass',line):
                    line = "\tinitial_mass = %g  !set by driver.py" % mass

                if re.search('initial\_z =',line):
                    line = "\tinitial_z = %g  !set by driver.py" % Zinit
                
                if re.search('Zbase =',line):
                    line = "\tZbase =%g  !set by driver.py" % Zinit

                if re.search('initial\_y',line):
                    line = "\tinitial_y = %g  !set by driver.py" % Yinit

                if re.search('mixing\_length\_alpha',line):
                    line = "\tmixing_length_alpha = %g  !set by driver.py" % amlt

                if fov_shell >0:
                    if re.search('overshoot\_scheme\(1\)',line):
                        line = "\tovershoot_scheme(1) = '%s' !set by driver.py " % "exponential" 
                    if re.search('overshoot\_zone\_type\(1\)',line):
                        line = "\tovershoot_zone_type(1) = '%s' !set by driver.py " % "any" 
                    if re.search('overshoot\_zone\_loc\(1\)',line):
                        line = "\tovershoot_zone_loc(1) = '%s' !set by driver.py " % "shell" 
                    if re.search('overshoot\_bdy\_loc\(1\)',line):
                        line = "\tovershoot_bdy_loc(1) = '%s' !set by driver.py " % "any" 
                    if re.search('overshoot\_f\(1\)',line):
                        line = "\tovershoot_f(1) = %g  !set by driver.py" %fov_shell
                    if re.search('overshoot\_f0\(1\)',line):
                        line = "\tovershoot_f0(1) = %g !set by driver.py" %fov0_shell

                else:
                    if re.search('overshoot\_scheme\(1\)',line):
                        line = "\t!overshoot_scheme(1) = '%s' !set by driver.py " % "" 
                    if re.search('overshoot\_zone\_type\(1\)',line):
                        line = "\t!overshoot_zone_type(1) = '%s' !set by driver.py " % "" 
                    if re.search('overshoot\_zone\_loc\(1\)',line):
                        line = "\t!overshoot_zone_loc(1) = '%s' !set by driver.py " % "" 
                    if re.search('overshoot\_bdy\_loc\(1\)',line):
                        line = "\t!overshoot_bdy_loc(1) = '%s' !set by driver.py " % "" 
                    if re.search('overshoot\_f\(1\)',line):
                        line = "\t!overshoot_f(1) = %g  !set by driver.py" % 0.
                    if re.search('overshoot\_f0\(1\)',line):
                        line = "\t!overshoot_f0(1) = %g !set by driver.py" % 0.               

                if fov_core >0:
                    if re.search('overshoot\_scheme\(2\)',line):
                        line = "\tovershoot_scheme(2) = '%s' !set by driver.py " % "exponential" 
                    if re.search('overshoot\_zone\_type\(2\)',line):
                        line = "\tovershoot_zone_type(2) = '%s' !set by driver.py " % "any" 
                    if re.search('overshoot\_zone\_loc\(2\)',line):
                        line = "\tovershoot_zone_loc(2) = '%s' !set by driver.py " % "core" 
                    if re.search('overshoot\_bdy\_loc\(2\)',line):
                        line = "\tovershoot_bdy_loc(2) = '%s' !set by driver.py " % "any" 
                    if re.search('overshoot\_f\(2\)',line):
                        line = "\tovershoot_f(2) = %g  !set by driver.py" %fov_core
                    if re.search('overshoot\_f0\(2\)',line):
                        line = "\tovershoot_f0(2) = %g !set by driver.py" %fov0_core

                else:
                    if re.search('overshoot\_scheme\(2\)',line):
                        line = "\t!overshoot_scheme(2) = '%s' !set by driver.py " % "" 
                    if re.search('overshoot\_zone\_type\(2\)',line):
                        line = "\t!overshoot_zone_type(2) = '%s' !set by driver.py " % "" 
                    if re.search('overshoot\_zone\_loc\(2\)',line):
                        line = "\t!overshoot_zone_loc(2) = '%s' !set by driver.py " % "" 
                    if re.search('overshoot\_bdy\_loc\(2\)',line):
                        line = "\t!overshoot_bdy_loc(2) = '%s' !set by driver.py " % "" 
                    if re.search('overshoot\_f\(2\)',line):
                        line = "\t!overshoot_f(2) = %g  !set by driver.py" % 0.
                    if re.search('overshoot\_f0\(2\)',line):
                        line = "\t!overshoot_f0(2) = %g !set by driver.py" % 0.   

                # smass = 'm{:03.0f}'.format(mass*100)
                header = 'index{:06.0f}'.format(index)

                if re.search('star\_history\_name',line):
                    output_history_name = header + '.history'
                    line = "\tstar_history_name = '%s'  !set by driver.py" % output_history_name

                if re.search('profile\_data\_prefix',line):
                    output_profile = header + 'profile'
                    line = "\tprofile_data_prefix = '%s' !set by driver.py " % output_profile 
                
                if re.search('profiles\_index\_name =', line):
                    output_index = header + 'profile.index'
                    line = "\tprofiles_index_name = '%s' !set by driver.py " % output_index 

                if ifsetfinalmodel:
                    if re.search('save\_model\_filename =', line):
                        output_final_model = header + 'final.mod'
                        line = "\tsave_model_filename = '%s' !set by driver.py " % output_final_model 

        print(line,file=outlist)

    outlist.close()
    inlist.close()

# read in input params
params = pd.read_csv('coarse_grid_input_params.txt')

# get from input which track to run for this simulation
index_this_run = int(sys.argv[1])
idx = (params['index'] >= index_this_run) & (params['index'] < (index_this_run+1))

# for track in tracks[idx]:
for ip, p in params.loc[idx,:].iterrows():

    # get input params for this simulation
    index, mass = p['index'], p['star_mass']
    Xinit, Yinit, Zinit, amlt = p['Xinit'], p['Yinit'], p['Zinit'], p['amlt']
    fov_shell, fov0_shell, fov_core, fov0_core = p['fov_shell'], p['fov0_shell'], p['fov_core'], p['fov0_core']
    mh = np.log10(Zinit/Xinit) - np.log10(0.0134/0.7381) # asplund 09 current solar abundance scale
    
    output_history_name = 'index{:06.0f}'.format(index)+'.history'
    output_final_model_name = 'index{:06.0f}final.mod'.format(index)
    output_profile_index_name = 'index{:06.0f}profile.index'.format(index)


    # if os.path.exists('../pre_rgb_tip/finalmodels/'+output_final_model_name): 
    #     print('File exists, skipping ', output_final_model_name)
    #     continue
    # else:
    #     print('Now calculating ', output_final_model_name)
    print('Now calculating ', output_final_model_name)

    # # # Step 1: modify inlist and run MESA.

    set_mesa_inlist(index, 'inlist_template', 'inlist', 
                    mass, Xinit, Yinit, Zinit, amlt, 
                    fov_shell, fov0_shell, fov_core, fov0_core, True)
    #os.system('\\rm -r LOGS; \\rm -r png; \\rm -r photos')

    print('------ MESA start ------')
    os.system('sh rn1 > mesa_output_index{:06.0f}.txt'.format(index))
    print('------ MESA done ------')



    # if os.path.exists(output_final_model_name):
    #     os.system('mv {:s} ../finalmodels/'.format(output_final_model_name))
    #     os.system('tar zcvf {:s}.tar.gz LOGS/index* '.format(output_history_name))
    #     os.system('mv {:s}.tar.gz ../history/'.format(output_history_name))
    #     # # List all files in the home directory
    #     # files = glob.glob(os.path.expanduser("photos/x*"))
    #     # if len(files) != 0:
    #     #     # Sort by modification time (mtime) descending
    #     #     latest_binary_photo = sorted(files, key=lambda t: -os.stat(t).st_mtime)[0]
    #     #     os.system('mv {:s} ../finalmodels/{:s}.x'.format(latest_binary_photo, output_final_model_name))
    # else:
    #     os.system('touch ../pre_rgb_tip/finalmodels/n{:s}'.format(output_final_model_name))
        

    # filepath = 'tmp/LOGS/'
    filepath = 'LOGS/'

    # # # Step 2: create a .h5 file to store history and frequencies. Only run if history file exists.
    if os.path.exists(filepath+output_history_name):

        # # read in models
        track = pd.read_fwf(filepath+output_history_name, skiprows=5, infer_nrows=10000)

        # # append grid initial parameters as new columns
        for col in ['index', 'Xinit', 'Yinit', 'Zinit', 'amlt', 'fov_shell', 'fov0_shell', 'fov_core', 'fov0_core']:
            track[col] = p[col]

        # # append phase as a new column
        phase = np.zeros(len(track))
        turnoffidx = track['center_h1'] < 1.e-7
        msidx = (10.0**(track['log_Lnuc']-track['log_L'])>0.99) & (~turnoffidx)
        pmsidx = (10.0**(track['log_Lnuc']-track['log_L'])<=0.99) & (~turnoffidx)
        sgidx = (turnoffidx) & (track['nu_max']>=300)
        rgbidx = (turnoffidx) & (track['nu_max']<300)
        hebidx = (turnoffidx) & ((track['center_he4']+track['center_he3']) <0.95)
        tfidx = (turnoffidx) & (~sgidx) & (~rgbidx) & (~hebidx)

        phase[pmsidx] = -1
        phase[msidx] = 0
        phase[sgidx] = 1
        phase[rgbidx] = 2
        phase[hebidx] = 3
        phase[tfidx] = -2
        track['phase'] = phase

        # # append log properties
        track['luminosity'] = 10.0**track['log_L']
        track['radius'] = 10.0**track['log_R']
        track['Teff'] = 10.0**track['log_Teff']

        # # append seismic scaling quantities
        Dnu_sun, numax_sun, Teff_sun = 135.1, 3090., 5772.
        track['Dnu_int'] = track['delta_nu']
        track.drop(columns=['delta_nu'])
        track['Dnu_scaling'] = track['star_mass']**0.5 * track['radius']**-1.5 * Dnu_sun
        track['numax'] = track['star_mass'] * track['radius']**-2.0 * (track['Teff']/Teff_sun)**-0.5 * numax_sun
        
        # # append surface quantities
        Zsun, Xsun = 0.0134, 0.7381 # 0.0134, 0.7381, a09 # 0.0169, 0.7345, gs98
        track['feh'] = np.log10((1-track['surface_h1']-track['surface_he4']-track['surface_he3'])/track['surface_h1']) - np.log10(Zsun/Xsun)


        # # assign a prior
        prior = 10.0**track['log_dt']
        track['prior'] = prior/np.sum(prior)


        sumPaths = [filepath+f for f in os.listdir(filepath) if f.endswith('.txt')]
        sumDirs = np.array([f.split('/index')[0]+'/' for f in sumPaths])
        sumNames = np.array([f.split('/')[-1] for f in sumPaths])


        # # read in radial mode frequencies
        seismicCols = ['l', 'n_p', 'n_g', 'n_pg', 'E_p', 'E_g', 'E_norm', 'Re(freq)', 'Re(freq)_corr']
        # seismicCols = ['l', 'n_p', 'n_g', 'n_pg', 'E_norm', 'freq']
        seismicData = [[] for i in range(len(seismicCols))]

        for it, t in track.loc[:,:].iterrows():
            if t['flag_gyre']:
                # sumFile = filepath+'index{:06.0f}profile{:0.0f}.data.FGONG.sum'.format(index, profileIndex)
                sumFile = f"index{p['index']:06.0f}.history.model{t['model_number']:06.0f}.txt"
                if len(sumDirs[sumNames==sumFile])==0:
                    for i in range(len(seismicData)):
                        seismicData[i].append(np.nan)
                    track.loc[it, 'flag_gyre'] = 0
                else:
                    sumPath = filepath + sumFile
                    sum = pd.read_fwf(sumPath)
                    for i in range(len(seismicData)):
                        seismicData[i].append( sum[seismicCols[i]].to_numpy() )
            else:
                for i in range(len(seismicData)):
                    seismicData[i].append(np.nan)


        # #  write out the table
        with h5py.File(f"index{p['index']:06.0f}.history.h5", 'w') as h5f:
            for col in track.columns:
                h5f.create_dataset(col, data=track[col].to_numpy())

            for it, t in track.iterrows():
                if t['flag_gyre']: 
                    for i in range(len(seismicCols)):
                        h5f.create_dataset('model_number{:0.0f}/{:s}'.format(t['model_number'], seismicCols[i]), data=seismicData[i][it])
                else:
                    continue

