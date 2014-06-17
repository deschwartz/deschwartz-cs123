#!/usr/bin/python
# Copyright 2012 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This is a modified version of job_terasort.py, with attribution above.
# It runs my MapReduce program to extract longitudinal problem solving data
# from Khan Academy's problem logs.


import os.path
import sys

from cfg import cfg
import common
import subprocess


def main():
  common.setup()

  # upload jar to hdfs
  jar = 'kaMapReduce.jar'
  upload("gs://ka_data/{0}".format(jar) , "/{0}".format(jar))
  
  # upload data to hdfs
  appends = [str(num).zfill(13) for num in range(0, 499)]
  p_logs = ["gs://ka_data/problem/" + append for append in appends]
    
  for i in range(0, 499):
    upload(p_log[i], "/problem/{0}".format(appends[i]))

  # actually run the job
  # may want to change number of tasks...
  num_tasks = 100
  job_args = ['kaMapReduce', '-Dmapred.reduce.tasks={0}'.format(num_tasks),
                '/', '/job_output/kaMapReduce']
  common.start_job(jar, job_args)

if __name__ == '__main__':
  main()
