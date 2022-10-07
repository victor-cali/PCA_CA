from sighandling import get_dataset_bbcic4_2b
from mne_features.feature_extraction import extract_features

# Import .mat file with data into an Epochs object from MNE library for neural signals
epochs = get_dataset_bbcic4_2b('./S4.mat')
# Drop channel Cz as only C3 and C4 will be used for the project
epochs.drop_channels(['Cz'])

# Create new filtered channels in mu and beta bands
fbands = ((7,13),(12,30))
filterbank = epochs.copy()
new_names = dict()
filtered_epochs = list()
for band in fbands:
    new_names.clear()
    for name in epochs.ch_names:
        if band is fbands[0]:
            new_names.update({name: f'{name}_mu'})
        else:
            new_names.update({name: f'{name}_beta'})
    subepochs = epochs.copy()
    subepochs.filter(band[0], band[1], method = 'iir', verbose = 50)
    subepochs.rename_channels(new_names)
    filtered_epochs.append(subepochs.copy())
filterbank.add_channels(filtered_epochs,force_update_info=True)
filterbank.drop_channels(['C3', 'C4'])

# Epochs from which features will be extracted
data = filterbank.get_data()
# Sampling Frequency of the data (250 Hz)
sampling_frequency = filterbank.info['sfreq']
# Features: Root Mean Value, Hjorth Mobility, Hjorth Complexity
features = ['rms', 'hjorth_mobility', 'hjorth_complexity']
# Channels: C3 and C4 in 2 different frequency bands
channels = filterbank.ch_names
# Feature extraction
dataset = extract_features(X = data, sfreq = sampling_frequency, selected_funcs = features, ch_names = channels, return_as_df = True)
# Rename columns rom features data set
dataset.columns = dataset.columns.map('_'.join).str.strip('_')
# Add categories (left and right hand movement)
dataset['targets'] = filterbank.events[:, -1]
# Save features dataset to .csv file
dataset.to_csv('./bci.csv', index = False)