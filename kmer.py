#######回归数据处理
import csv
from itertools import product
#设置kmer的k
k=5

file_path = './regression/x-2k_sequence.fa'# 读取文件路径
file_name='./regression/x-2k_sequence'+str(k)+'mer.csv'#kmer特征文件名

def read_lines_as_list(file_path):
    lines = []
    with open(file_path, 'r') as file:
        for line in file:
            # 去除行尾的换行符
            line = line.rstrip('\n')
            # 将行添加到列表中
            if not (line.startswith('>') or 'N' in line):
                # 将非'>'开头的行添加到列表中
                lines.append(line)
    return lines



# 读取每一条序列为列表元素
sequences = read_lines_as_list(file_path)
#统计其中不包含N的序列号
valid_sequences = [i+1 for i, seq in enumerate(sequences) if 'N' not in seq]
with open('no_N_index.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    for num in valid_sequences:
        writer.writerow([num])

        #提取单条序列的kmer
def extract_kmers(dna_sequence, k):
    kmers = []
    seq_length = len(dna_sequence)
    
    for i in range(seq_length - k + 1):
        kmer = dna_sequence[i:i+k]
        kmers.append(kmer)
    return kmers




#统计单条序列中kmer出现的频率
def count_kmer_frequency(sequence, k):
    combinations = list(product('ATCG', repeat=k))
    # 创建空字典
    kmer_counts = {}
    # 使用每个组合作为键，初始化字典的值为0
    for combination in combinations:
        kmer = ''.join(combination)
        kmer_counts[kmer] = 0
    for i in range(len(sequence) - k + 1):
        kmer = sequence[i:i+k]
        kmer_counts[kmer] += 1
    #将每个频次转化为频率
    for kmer, value in kmer_counts.items():
        # 修改值
        kmer_counts[kmer] = value/(len(sequence)-k+1)
    return kmer_counts

sequence_kmer_frequency=[]
for sequence in sequences:
    kmer_frequency=count_kmer_frequency(sequence, k)#计算当前序列的各个kmer频次
    sequence_kmer_frequency.append(kmer_frequency)

#将每条序列的kmer特征向量提取出来并写入文件
for i in range(len(sequence_kmer_frequency)):
    sequence_kmer_frequency[i]=list(sequence_kmer_frequency[i].values())

len(sequence_kmer_frequency)
def write_list_to_csv(filename, data):
    with open(filename, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerows(data)

write_list_to_csv(file_name,sequence_kmer_frequency)