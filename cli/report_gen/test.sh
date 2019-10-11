#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
msg="colecho -b"

html_gen="generate_page.py"
method_gen="../extract_methods/extract_methods.nim.bin"
flowchart_gen="../flowchart_generator/flowchart_generator.nim.bin"

in_file_dir="test"
split_methods_dir="methods.tmp.d"
flowchart_images="flowcharts.tmp.d"
final_dir="output.tmp.d"
image_ext="png"

mkdir -p $final_dir
mkdir -p $flowchart_images
mkdir -p $split_methods_dir

find . -type f -name "*.tmp.*" -print0 | xargs -0 rm

colecho -g -i:3 "Starting"


$msg -i:1 "Splitting files in directory '$in_file_dir'"
while read -r class_file
do
    $msg "Working with $class_file"
    ./$method_gen \
        --verbose \
        --input-file:"$in_file_dir/$class_file" \
        --output-dir:"$split_methods_dir" \
        --maxlen:200 \
        --ignored-methods:"initComponents,main" \
        --debug

done < <(find $in_file_dir -type f -printf "%f\n")

$msg -i:1 "Formatting generated files"
while read -r method_file
do
    clang-format -i -style=file -verbose "$method_file"
done < <(find $split_methods_dir -type f)


$msg -i:1 "Generating flowcharts for files in directory '$split_methods_dir'"
while read -r method_file
do
    $msg -I:4 "Working with '$method_file'"
    abs_method="$split_methods_dir/$method_file"

    $msg -I:8 "generate dot file ..."
    if ! ./$flowchart_gen \
         --input:"$abs_method" \
         --output:"$flowchart_images/$method_file.dot"
       then
        $msg -I:8 -e "Error ocurred duing file parse:"
        bat "$abs_method"
        exit
    fi

    $msg -I:8 "generate synt file ..."
    ./$flowchart_gen \
        --dump-tree \
        --input:"$abs_method" \
        --output:"$flowchart_images/$method_file.synt"

done < <(find $split_methods_dir -type f -printf "%f\n")

$msg -i:1 "Generating images for files in directory '$flowchart_images'"
while read -r dot_file
do
    $msg -I:4 "Working with '$dot_file'"
    if ! dot -T$image_ext "$dot_file" > "$dot_file.$image_ext"
       then
           $msg -I:8 -e "Error ocurred during file conversion"
           bat "$dot_file"
           exit
    fi
done < <(find $flowchart_images -type f -name "*.dot")

$msg -i:1 "Generating debug html page"
./$html_gen \
    --output-file="out.tmp.html" \
    --image-suffix=".dot.$image_ext" \
    --image-prefix="$flowchart_images/" \
    --syntax-suffix=".synt" \
    --syntax-prefix="$flowchart_images/" \
    --files-glob="$split_methods_dir/*" \
    --files-prefix="$split_methods_dir/"

image_files=""

while read -r flowchart
do
    $msg -I:4 "Renaming $flowchart_images/$flowchart"
    oldname=$flowchart
    newname="${oldname//'.'/'_'}.$image_ext"
    cp "$flowchart_images/$oldname" "$final_dir/$newname"
    image_files="$image_files,$newname"
done < <(find $flowchart_images -type f -name "*.$image_ext" -printf "%f\n")


$msg -i:1 "Running report template"

task_formulation=$(find "$in_file_dir" -name "task.txt")
source_files=$(find "$in_file_dir" -name "*.java" -printf \
                    "$in_file_dir/%f\n" \
                   | tee /dev/tty \
                   | tr '\n' ',')

cp $flowchart_images/*.$image_ext $final_dir


tpage \
    --define "files=$source_files" \
    --define "task=$task_formulation" \
    --define "image_files=$image_files" \
    report_template > "$final_dir/report.tex"


$msg -i:1 "Building pdf"

cd $final_dir

# latexmk -C report.tex
# latexmk -latexoption="-shell-escape" \
#         -pdflua --interaction=nonstopmode report.tex > \
#         /dev/null

$msg -i:3 "Done"
