# ~/.emacs.d/audio_recorder.py
import sounddevice as sd
import soundfile as sf
import numpy as np
import time
import sys
import tempfile

device = int(sys.argv[1])
output = sys.argv[2]
samplerate = 44100
channels = 1

print("Recording... Press Enter to stop.")
duration = []

# Use a NumPy array to store the audio data
audio_data = []

def callback(indata, frames, time_info, status):
    if status:
        print(status, file=sys.stderr)
    # Append the incoming audio data to our list
    audio_data.append(indata.copy())

# Start recording
with sd.InputStream(samplerate=samplerate, channels=channels, callback=callback, device=device):
    start = time.time()
    input()  # Wait for Enter key
    end = time.time()
    duration.append(end - start)

# Convert the list of audio chunks to a single numpy array
if audio_data:
    # Concatenate all audio chunks
    combined_data = np.vstack(audio_data)

    # Save to the output file
    if output.lower().endswith('.ogg'):
        # For OGG format, use VORBIS subtype
        sf.write(output, combined_data, samplerate, format='OGG', subtype='VORBIS')
    else:
        # Default to WAV with PCM_16 for other cases
        sf.write(output, combined_data, samplerate, format='WAV', subtype='PCM_16')

print(f"DURATION:{duration[0]}")
