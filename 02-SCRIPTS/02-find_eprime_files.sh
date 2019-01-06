#!bin/bash

#-------- 
basedir=/Volumes/ENCRYPTED
copydir=$basedir/01-COPYDIR/TestRoom2 #must have /LOCATION (i.e.TestRoom1/TestRoom2)
task="Antisaccade"
#tasks="n-back \
#Attention \
#Antisaccade"
copy_etxts=1
copy_edats=1
#-------- 
#for task in $tasks; do
	
	outdir=$basedir/$task
	
	copy_etxt=$outdir/5-COPY_etxt
	copy_edat=$outdir/6-COPY_edat


	if [ "$task" == "n-back" ]; then
		exp_lines=6113 #6442/6769
	elif [ "$task" == "Attention" ]; then
		exp_lines=13289
	elif [ "$task" == "Antisaccade" ]; then
		exp_lines=6333
	else
		echo "ERROR: edit script to include task. Quitting"
		exit 1
	fi

	dirstruct="1-ID_dataframes \
	2-COMPLETE_etxt \
	3-COMPLETE_edat \
	4-ERROR_check \
	5-COPY_etxt \
	6-COPY_edat \
	7-FATAL \
	8-LOGS \
	"

	filelog=$outdir/ALL_FILES.log

	if [ ! -d "$outdir" ]; then
		echo "creating ${outdir}"
		mkdir ${outdir}
	fi

	for d in $dirstruct; do
		if [ ! -d $outdir/$d ]; then
			echo "creating ${outdir}/${d}"
			mkdir $outdir/$d
		fi
	done

	if [ ! -e "$filelog" ]; then
		echo "creating ${filelog}"
		touch $filelog
	fi


	#---copy etxts
	if [ "$copy_etxts" == 1 ]; then
		printf "\e[1;34mcopying ${task}*.txt to ${copy_etxt}. This might take a while. PLEASE WAIT\e[0m\n"
		find $copydir \
		-type f \
		-name "${task}*txt" \
		-exec cp \
		-n \
		-p {} $copy_etxt \;
		
		echo "DONE"
	fi


	#---copy edats
	if [ "$copy_edats" == 1 ]; then
		printf "\e[1;34mcopying ${task}*.edat* to ${copy_edat}. This might take a while. PLEASE WAIT\e[0m\n"
		find $copydir \
		-type f \
		-name "${task}*edat*" \
		-exec cp \
		-n \
		-p {} $copy_edat \;
		
		echo "DONE"
	fi


	#clean filenames with spaces
	printf "\e[1;34mcleaning filenames\e[0m\n"
	find $copy_etxt/*\ * > $copy_etxt/tmplines.txt
	nclean=`cat $copy_etxt/tmplines.txt | wc -l`
	if [ "$nclean" -gt 0 ]; then
		echo "cleaning ${nclean} filenames"
		for c in `seq $nclean`; do
			oldname=`sed -n "${c}p" $copy_etxt/tmplines.txt`
			mv "$oldname" "${oldname// /_}"
		done
	fi
	rm $copy_etxt/tmplines.txt

	find $copy_edat/*\ * > $copy_edat/tmplines.txt
	nclean=`cat $copy_edat/tmplines.txt | wc -l`
	if [ "$nclean" -gt 0 ]; then
		echo "cleaning ${nclean} filenames"
		for c in `seq $nclean`; do
			oldname=`sed -n "${c}p" $copy_edat/tmplines.txt`
			mv "$oldname" "${oldname// /_}"
		done
	fi
	rm $copy_edat/tmplines.txt



	printf "\e[1;34mextracting etxt info\nnewfiles:\e[0m\n"
	for f in $copy_etxt/*.txt; do

		#sub
		tmp1=`grep "Subject" $f | head -n1`
		tmp1=`echo $tmp1 | sed 's/.*://'`
		subcheck="$(echo $tmp1 | tr -d '\r')"


		#date
		tmp2=`grep "SessionDate" $f | head -n1`
		tmp2=`echo $tmp2 | sed 's/.*://'`
		mm=`echo $tmp2 | cut -c1-2`
		dd=`echo $tmp2 | cut -c4-5`
		yyyy=`echo $tmp2 | cut -c7-10`
		datecheck=`echo "${yyyy}-${mm}-${dd}"`

		
		#time
		tmp3=`grep "SessionTime" $f | head -n1`
		tmp3=`echo $tmp3 | rev | cut -c1-9 | rev`
		timecheck="$(echo $tmp3 | tr -d '\r')"
		

		#new etxt file encoding not grep compatible
		if [ -z "$subcheck" ]; then 
			
			#sub
			tmp1=`sed -n '17p' $f`
			tmp1=`echo $tmp1 | sed 's/.*://'`
			subcheck="$(echo $tmp1 | tr -d '\r')"
		

			#date
			tmp2=`sed -n '14p' $f`
			tmp2=`echo $tmp2 | sed 's/.*://'`
			mm=`echo $tmp2 | cut -c1-2`
			dd=`echo $tmp2 | cut -c4-5`
			yyyy=`echo $tmp2 | cut -c7-10`
			datecheck=`echo "${yyyy}-${mm}-${dd}"`


			#time
			tmp3=`sed -n '15p' $f`
			tmp3=`echo $tmp3 | rev | cut -c1-9 | rev`
			timecheck="$(echo $tmp3 | tr -d '\r')"
		fi



		#append to filelog if new
		msg="`basename ${f}` ; ${datecheck} ; ${timecheck}"
		greps=`grep "$msg" $filelog`


		if [ -e "$outdir/FATAL.log" ]; then
			fatalgreps=`grep "$msg" $outdir/FATAL.log`
			if [ ! -z "$fatalgreps" ]; then

				#remove ext
				minusext=`echo $f | rev | cut -c5- | rev`
				minusext=$(basename $minusext)
				strcut=`echo $minusext | rev | cut -c1-2 | rev`	
				
				dest_fatal=$outdir/7-FATAL/fatal_${subcheck}${strcut}
				
				if [ -e "$dest_fatal/`basename ${f}`" ]; then
					echo "`basename ${f}` previously deemed FATAL. Removing from $copy_etxt"
					rm $f
				fi

				#search for edat
				edatthere=`find $copy_edat/$minusext*edat*`


				if [ ! -z "$edatthere" ]; then 
					edattrue=`basename $edatthere`
					if [ -e "$dest_fatal/$edattrue" ]; then
						echo "${edattrue} previously deemed FATAL. Removing from ${copy_edat}"
						rm $copy_edat/$edattrue
					fi
				fi
			fi
		fi	
		

		if [ -z "$greps" ]; then
			
			#check fatalities
			#fatal = linecount < (exp_lines / 4)
			linecount=`wc -l $f | awk '{print $1}'`
			
			if [ "$linecount" -le `echo $(($exp_lines / 3))` ]; then
				
				echo "`basename ${f}` = FATAL"
				
				#remove ext
				minusext=`echo $f | rev | cut -c5- | rev`
				minusext=$(basename $minusext)
				strcut=`echo $minusext | rev | cut -c1-2 | rev`	
				
				dest_fatal=$outdir/7-FATAL/fatal_${subcheck}${strcut}
				if [ ! -d "$dest_fatal" ]; then
					echo "creating ${dest_fatal}"
					mkdir $dest_fatal
				fi

				
				echo "moving `basename ${f}` to ${dest_fatal}"
				mv $f $dest_fatal
				reason="FATAL\nreason: linecount = ${linecount} (expected ${exp_lines})"
				echo $reason >> $dest_fatal/reason.txt


				sessionD=`echo "${datecheck//-/}"`
				sessionT=`echo "${timecheck//:/}"`
				lifecyclelog="`basename ${f}`_${sessionD}_${sessionT}.log"
				echo $reason >> $outdir/8-LOGS/$lifecyclelog

				#search for edat
				edatthere=`find $copy_edat/$minusext*edat*`
				edatthere=`basename $edatthere`

				if [ ! -z "$edatthere" ]; then 
					echo "moving ${edatthere} to ${dest_fatal}"
					mv $copy_edat/$edatthere $dest_fatal

					#add to fatal log
					msg="`basename ${f}` ; ${datecheck} ; ${timecheck} ; `basename ${copydir}` ; `basename ${dest_fatal}` ; linecount ; $edatthere"
					echo $msg >> $outdir/FATAL.log
					#add fatal to log
					echo $msg; echo $msg >> $filelog
				
				else

					#add to fatal log
					msg="`basename ${f}` ; ${datecheck} ; ${timecheck} ; `basename ${copydir}` ; `basename ${dest_fatal}` ; linecount"
					echo $msg >> $outdir/FATAL.log
					#add fatal to log
					echo $msg; echo $msg >> $filelog
				fi

			else 

				#add to log
				msg="`basename ${f}` ; ${datecheck} ; ${timecheck} ; `basename ${copydir}`" 
				echo $msg; echo $msg >> $filelog
			fi	
		fi
	done

	echo "DONE"

#done
