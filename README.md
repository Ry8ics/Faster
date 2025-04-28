# Faster
Manipulate Openfast input files, run the analyses automatically in parallel and display the results in a user controlled manner.

The repository is developed in Visual Studio environment ready to be cloned.
The proposed tools are Visual Studio Community 2022 17.9 along with Intel OneAPI Base Toolkit and Intel OneAPI HPC Toolkit. 
When installing Visual Studio Community the option "Desktop development with C++" must be checked on.

If you do not care to compile the code, then just download Faster_including_user_manual_v2.zip under Releases and you are good to go.

Read the attached user manual to learn how to create input files to Faster.
Open a Windows Terminal and and run Faster by issuing the command "faster<input_file.inp>output_file.out"
where the textfile input_file.inp contains the required commands to faster. A basic input file to Faster may look like:

id SIMP expand 1 'my simple faster'

maestro openfast IEA-10.0-198-RWT.fst

anal on

list stat GenPwr

which when run will apply the openfast file IEA-10.0-198-RWT.fst and list the statistics for the channel GenPwr.
