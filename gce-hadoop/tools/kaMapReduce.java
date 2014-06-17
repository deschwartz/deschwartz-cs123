package org.myorg;

import java.io.IOException;
import java.util.StringTokenizer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Comparator;
import java.util.TreeSet;
import java.util.Iterator;

import org.apache.commons.lang3.StringUtils;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.util.GenericOptionsParser;

public class kaMapReduce {
    public static class myMapper
	extends Mapper<Object, Text, Text, Text> {

        // Mapper function that takes (key, value) and uses the
        // output object to output data.
		
		// Constants to index into a raw line
		int ID = 0;
		int CORRECT = 1;
		int TIMESTAMP = 2;
		int TIMETAKEN = 3;
		int HINT = 8;
		int TYPE = 10;

		public void map(Object key, Text value, Context context
				) throws IOException, InterruptedException {
		    String[] log = value.toString().split(",");
	    
			// extract the user id
			String user_id = log[ID];
		
			// shorten the data I want, put it back into a string
			String correct = log[CORRECT].equals("true") ? "1" : "0";
			String hint = log[HINT].equals("true") ? "1" : "0";
		
			String data = correct + ',' + log[TIMESTAMP] + ',' + log[TIMETAKEN] + 
						  ',' + hint + ',' + log[TYPE];

		    // write to context
		    context.write(new Text(user_id), new Text(data));
		}
    }
	
	public static class logDateComparator implements Comparator<String> {
		
		public int compare(String x, String y) {
			String[] x_arr = x.split(",");
			String[] y_arr = y.split(",");
			
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss z");
			
			try {
				return df.parse(x_arr[1]).compareTo(df.parse(y_arr[1]));
			} catch (Exception E) {
				return 0;
			}
		}
	}

    public static class myReducer
	extends Reducer<Text, Text, Text, Text> {
		
		// Constants to index into a line given by the mapper
		int CORRECT = 0;
		int TIMESTAMP = 1;
		int TIMETAKEN = 2;
		int HINT = 3;
		int TYPE = 4;

	// The reducer expects a key of type Text (a name) and 
	// a list of values obtained via iterator (here it's a
	// list of Text).
	// The reducer will output keys of type Text and values
	// of type Text.

		public void reduce(Text key, Iterable<Text> values,
				   Context context
				   ) throws IOException, InterruptedException {	
			// initialize arrays for values I'm keeping track of
			// each element of each array represents that variable for 
			// a different time interval (d1, d2t7, w2, w3t4, m2, m3, m4, m5, m6, m12)
			int[] num_correct = new int[10];
			int[] num_problems = new int[10];
			int[] current_wrong_streak = new int[10];
			int[] longest_wrong_streak = new int[10];
			long[] total_timetaken = new long[10];
			int[] num_hints = new int[10];
			int[] num_tutorial = new int[10];
			int[] num_mastery = new int[10];
		
			// initialize other 
			String type = new String();
			String[] log_arr = new String[12];
			long elapsed_time = 0;
			Boolean correct = false;
			int interval = 0;
		
			// indices for each time interval (same for all arrays)
			int D1 = 0;
			int D2T7 = 1;
			int W2 = 2;
			int W3T4 = 3;
			int M2 = 4;
			int M3 = 5;
			int M4 = 6;
			int M5 = 7;
			int M6 = 8;
			int M12 = 9;
		
			// sort the logs by timestamp (via logDateComparator)		
			TreeSet<String> sorted_logs = new TreeSet<String>(new logDateComparator());
		
			for (Text val : values) {
				// add to tree (as a string)
				sorted_logs.add(val.toString());
			}
		
			// extract the data I want
			String earliest = sorted_logs.first().split(",")[TIMESTAMP];
			String latest = sorted_logs.last().split(",")[TIMESTAMP];
		
			Iterator<String> logs_iterator = sorted_logs.iterator();
			
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss z");
		
			while (logs_iterator.hasNext()) {
				// log_arr
				log_arr = logs_iterator.next().split(",");
			
				// figure out which time interval we're in...
				//      continue if we're in months 7-11
				//      break if we're past month 12 (into year 2)
				try {
					elapsed_time = df.parse(log_arr[TIMESTAMP]).getTime() - df.parse(earliest).getTime();
					if (elapsed_time < df.parse("1970-01-02 00:00:00 UTC").getTime()) {
						interval = D1;
					} 
					else if (elapsed_time < df.parse("1970-01-08 00:00:00 UTC").getTime()) {
						interval = D2T7;
					}
					else if (elapsed_time < df.parse("1970-01-15 00:00:00 UTC").getTime()) {
						interval = W2;
					} 
					else if (elapsed_time < df.parse("1970-01-31 00:00:00 UTC").getTime()) {
						interval = W3T4;
					}
					else if (elapsed_time < df.parse("1970-02-28 00:00:00 UTC").getTime()) {
						interval = M2;
					}
					else if (elapsed_time < df.parse("1970-03-31 00:00:00 UTC").getTime()) {
						interval = M3;
					} 
					else if (elapsed_time < df.parse("1970-04-30 00:00:00 UTC").getTime()) {
						interval = M4;
					}
					else if (elapsed_time < df.parse("1970-05-31 00:00:00 UTC").getTime()) {
						interval = M5;
					}
					else if (elapsed_time < df.parse("1970-01-15 00:00:00 UTC").getTime()) {
						interval = M6;
					}
					else if (elapsed_time < df.parse("1970-11-30 00:00:00 UTC").getTime()) {
						continue;
					}
					else if (elapsed_time < df.parse("1970-12-31 00:00:00 UTC").getTime()) {
						interval = M12;
					}
					else {
						break;
					}
					
				} catch (Exception e) {
					
				}
						
				// num_correct
				num_correct[interval] += log_arr[CORRECT].equals("1") ? 1 : 0;
			
				// num_problems
				num_problems[interval] += 1;
			
				// longest_wrong_streak
				if (!correct) {
					current_wrong_streak[interval] += 1;
					if (current_wrong_streak[interval] > longest_wrong_streak[interval]) {
						longest_wrong_streak[interval] = current_wrong_streak[interval];
					}
				} 
				else {
					current_wrong_streak[interval] = 0;
				}
			
				// total_timetaken
				total_timetaken[interval] += Long.parseLong(log_arr[TIMETAKEN]);
			
				// num_hints
				num_hints[interval] += log_arr[HINT].equals("1") ? 1 : 0;
			
				// num_master, num_practice
				type = log_arr[TYPE];
				if (type.equals("practice") || type.equals("library") || type.equals("practice.tutorial")) {
					num_tutorial[interval] += 1;
				}
				else if (type.contains("mastery")) {
					num_mastery[interval] += 1;
				} 
			}
		
			// 
			// write summary line to context (to send to reducer)
			String data = StringUtils.join(num_correct, ',') + "," + 
						  StringUtils.join(num_problems, ',') + "," +
						  StringUtils.join(longest_wrong_streak, ',') + "," + 
						  StringUtils.join(total_timetaken, ',') + "," + 
						  StringUtils.join(num_hints, ',') + "," +
						  StringUtils.join(num_tutorial, ',') + "," + 
						  StringUtils.join(num_mastery, ',') + "," +
						  earliest + "," + 
						  latest;
				
			context.write(key, new Text(data));
		}
    }

    public static void main(String[] args) throws Exception {
		Configuration conf = new Configuration();
		
		String[] otherArgs = new GenericOptionsParser(conf, args).getRemainingArgs();
		if (otherArgs.length != 2) {
		    System.err.println("Usage: kaMapReduce <inPath> <outPath>");
		    System.exit(2);
		}
		// Creates a MapReduce job and links it to our class
		Job job = Job.getInstance(conf);
		job.setJarByClass(kaMapReduce.class);

		// Sets the mapper/combiner/reducer
		job.setMapperClass(myMapper.class);
		job.setReducerClass(myReducer.class);
	
		// Output types
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(Text.class);
		
		// input/output formats
		job.setInputFormatClass(TextInputFormat.class);
		job.setOutputFormatClass(TextOutputFormat.class);

		// The paths of these IO are from application arguments
		FileInputFormat.addInputPath(job, new Path(otherArgs[0]));
		FileOutputFormat.setOutputPath(job, new Path(otherArgs[1]));

		//Finally, run the job!
		System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
	
