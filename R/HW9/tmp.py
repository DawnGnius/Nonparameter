# -*- coding: UTF-8 -*-
import matplotlib.pyplot as plt
import numpy as np
from scipy import stats
import math
import sys
import random
 
# reload(sys)
# sys.setdefaultencoding('utf-8')
 
 
parameter_dict = {}
parameter_dict["Mu_1"] = np.array([0, 0])
parameter_dict["Sigma_1"] = np.array([[1, 0], [0, 1]])
parameter_dict["Mu_2"] = np.array([0, 0])
parameter_dict["Sigma_2"] = np.array([[1, 0], [0, 1]])
parameter_dict["Pi_weight"] = 0.5
parameter_dict["gama_list"] = []
 
 
def set_parameter(mu_1, sigma_1, mu_2, sigma_2, pi_weight):
    parameter_dict["Mu_1"] = mu_1
    parameter_dict["Mu_1"].shape = (2, 1)
    parameter_dict["Sigma_1"] = sigma_1
    parameter_dict["Mu_2"] = mu_2
    parameter_dict["Mu_2"].shape = (2, 1)
    parameter_dict["Sigma_2"] = sigma_2
    parameter_dict["Pi_weight"] = pi_weight
 
 
def PDF(data, Mu, sigma):
    """
    二元正态分布概率密度函数
    :param data: 一个二维数据点,ndarray
    :param Mu: 均值,ndarray
    :param Sigama: 协方差阵ndarray
    :return:该数据点的概率密度值
    """
    sigma_sqrt = math.sqrt(np.linalg.det(sigma))  # 协方差矩阵绝对值的1/2次
    sigma_inv = np.linalg.inv(sigma)  # 协方差矩阵的逆
    data.shape = (2, 1)
    Mu.shape = (2, 1)
    minus_mu = data - Mu
    minus_mu_trans = np.transpose(minus_mu)
    res = (1.0 / (2.0 * math.pi * sigma_sqrt)) * math.exp(
        (-0.5) * (np.dot(np.dot(minus_mu_trans, sigma_inv), minus_mu)))
    return res
 
def E_step(Data):
    """
    E-step: compute responsibilities
    计算出本轮gama_list
    :param Data:一系列二维的数据点
    :return:gama_list
    """
    # 协方差矩阵
    sigma_1 = parameter_dict["Sigma_1"]
    sigma_2 = parameter_dict["Sigma_2"]
    pw = parameter_dict["Pi_weight"]
    mu_1 = parameter_dict["Mu_1"]
    mu_2 = parameter_dict["Mu_2"]
 
    parameter_dict["gama_list"] = []
    for point in Data:
        gama_i = (pw * PDF(point, mu_2, sigma_2)) / (
            (1.0 - pw) * PDF(point, mu_1, sigma_1) + pw * PDF(point, mu_2, sigma_2))
        parameter_dict["gama_list"].append(gama_i)
 
 
def M_step(Data):
    """
    M-step: compute weighted means and variances
    更新均值与协方差矩阵
    在此例中，   gama_i对应Mu_2,Var_2
                (1-gama_i)对应Mu_1,Var_1
    :param X:一系列二维的数据点
    :return:
    """
    N_1 = 0
    N_2 = 0
    for i in range(len(parameter_dict["gama_list"])):
        N_1 += 1.0 - parameter_dict["gama_list"][i]
        N_2 += parameter_dict["gama_list"][i]
 
 
    # 更新均值
    new_mu_1 = np.array([0, 0])
    new_mu_2 = np.array([0, 0])
    for i in range(len(parameter_dict["gama_list"])):
        new_mu_1 = new_mu_1 + Data[i] * (1 - parameter_dict["gama_list"][i]) / N_1
        new_mu_2 = new_mu_2 + Data[i] * parameter_dict["gama_list"][i] / N_2
 
    # 很重要，numpy对一维向量无法转置，必须指定shape
    new_mu_1.shape = (2, 1)
    new_mu_2.shape = (2, 1)
 
    new_sigma_1 = np.array([[0, 0], [0, 0]])
    new_sigma_2 = np.array([[0, 0], [0, 0]])
    for i in range(len(parameter_dict["gama_list"])):
        data_tmp = [0, 0]
        data_tmp[0] = Data[i][0]
        data_tmp[1] = Data[i][1]
        vec_tmp = np.array(data_tmp)
        vec_tmp.shape = (2, 1)
        new_sigma_1 = new_sigma_1 + np.dot((vec_tmp - new_mu_1), (vec_tmp - new_mu_1).transpose()) * (1.0 - parameter_dict["gama_list"][i]) / N_1
        new_sigma_2 = new_sigma_2 + np.dot((vec_tmp - new_mu_2), (vec_tmp - new_mu_2).transpose()) * parameter_dict["gama_list"][i] / N_2
        # print np.dot((vec_tmp-new_mu_1), (vec_tmp-new_mu_1).transpose())
    new_pi = N_2 / len(parameter_dict["gama_list"])
 
 
    # 更新类变量
    parameter_dict["Mu_1"] = new_mu_1
    parameter_dict["Mu_2"] = new_mu_2
    parameter_dict["Sigma_1"] = new_sigma_1
    parameter_dict["Sigma_2"] = new_sigma_2
    parameter_dict["Pi_weight"] = new_pi
 
 
def EM_iterate(iter_time, Data, mu_1, sigma_1, mu_2, sigma_2, pi_weight, esp=0.0001):
    """
    EM算法迭代运行
    :param iter_time: 迭代次数，若为None则迭代至约束esp为止
    :param Data:数据
    :param esp:终止约束
    :return:
    """
 
    set_parameter(mu_1, sigma_1, mu_2, sigma_2, pi_weight)
    if iter_time == None:
        while (True):
            old_mu_1 = parameter_dict["Mu_1"].copy()
            old_mu_2 = parameter_dict["Mu_2"].copy()
            E_step(Data)
            M_step(Data)
            delta_1 = parameter_dict["Mu_1"] - old_mu_1
            delta_2 = parameter_dict["Mu_2"] - old_mu_2
            if math.fabs(delta_1[0]) < esp and math.fabs(delta_1[1]) < esp and math.fabs(
                    delta_2[0]) < esp and math.fabs(delta_2[1]) < esp:
                break
    else:
        for i in range(iter_time):
            pass
 
 
def EM_iterate_trajectories(iter_time, Data, mu_1, sigma_1, mu_2, sigma_2, pi_weight, esp=0.0001):
    """
    EM算法迭代运行,同时画出两个均值变化的轨迹
    :param iter_time:迭代次数，若为None则迭代至约束esp为止
    :param Data: 数据
    :param esp: 终止约束
    :return:
    """
    mean_trace_1 = [[], []]
    mean_trace_2 = [[], []]
 
    set_parameter(mu_1, sigma_1, mu_2, sigma_2, pi_weight)
    if iter_time == None:
        while (True):
            old_mu_1 = parameter_dict["Mu_1"].copy()
            old_mu_2 = parameter_dict["Mu_2"].copy()
            E_step(Data)
            M_step(Data)
            delta_1 = parameter_dict["Mu_1"] - old_mu_1
            delta_2 = parameter_dict["Mu_2"] - old_mu_2
 
            mean_trace_1[0].append(parameter_dict["Mu_1"][0][0])
            mean_trace_1[1].append(parameter_dict["Mu_1"][1][0])
            mean_trace_2[0].append(parameter_dict["Mu_2"][0][0])
            mean_trace_2[1].append(parameter_dict["Mu_2"][1][0])
            if math.fabs(delta_1[0]) < esp and math.fabs(delta_1[1]) < esp and math.fabs(delta_2[0]) < esp and math.fabs(delta_2[1]) < esp:
                break
    else:
        for i in range(iter_time):
            pass

    # draw, 这两幅图说了什么?
    plt.subplot(121)
    plt.xlim(xmax=5, xmin=2)
    plt.ylim(ymax=90, ymin=60)
    plt.xlabel("eruptions")
    plt.ylabel("waiting")
    plt.plot(mean_trace_1[0], mean_trace_1[1], 'r-')
    plt.plot(mean_trace_1[0], mean_trace_1[1], 'b^')
 
    plt.subplot(122)
    plt.xlim(xmax=4, xmin=0)
    plt.ylim(ymax=60, ymin=40)
    plt.xlabel("eruptions")
    plt.ylabel("waiting")
    plt.plot(mean_trace_2[0], mean_trace_2[1], 'r-')
    plt.plot(mean_trace_2[0], mean_trace_2[1], 'bo')

    plt.show()
 
 
def EM_iterate_times(Data, mu_1, sigma_1, mu_2, sigma_2, pi_weight, esp=0.0001):
    # 返回迭代次数
    set_parameter(mu_1, sigma_1, mu_2, sigma_2, pi_weight)
    iter_times = 0
    while (True):
        iter_times += 1
        old_mu_1 = parameter_dict["Mu_1"].copy()
        old_mu_2 = parameter_dict["Mu_2"].copy()
        E_step(Data)
        M_step(Data)
        delta_1 = parameter_dict["Mu_1"] - old_mu_1
        delta_2 = parameter_dict["Mu_2"] - old_mu_2
        if math.fabs(delta_1[0]) < esp and math.fabs(delta_1[1]) < esp and math.fabs(
                delta_2[0]) < esp and math.fabs(delta_2[1]) < esp:
            break
    return iter_times
 
 
def task_1():
    # 读取数据，猜初始值,执行算法
    Data_list = []
    with open("D:/Documents/Codes/Nonparameter/R/HW9/old_faithful_geyser_data.txt", 'r') as in_file:
        for line in in_file.readlines():
            point = []
            point.append(float(line.split()[0]))
            point.append(float(line.split()[1]))
            Data_list.append(point)
    Data = np.array(Data_list)

    # initial
    Mu_1 = np.array([3, 60])
    Sigma_1 = np.array([[10, 0], [0, 10]])
    Mu_2 = np.array([1, 30])
    Sigma_2 = np.array([[10, 0], [0, 10]])
    Pi_weight = 0.5
 
    EM_iterate_trajectories(None, Data, Mu_1, Sigma_1, Mu_2, Sigma_2, Pi_weight)
 
 
def task_2():
    """
    执行50次，看迭代次数的分布情况
    这里协方差矩阵都取[[10, 0], [0, 10]]
    mean值在一定范围内随机生成50组数
    :return:
    """
    # 读取数据，猜初始值,执行算法
    Data_list = []
    with open("D:/Documents/Codes/Nonparameter/R/HW9/old_faithful_geyser_data.txt", 'r') as in_file:
        for line in in_file.readlines():
            point = []
            point.append(float(line.split()[0]))
            point.append(float(line.split()[1]))
            Data_list.append(point)
    Data = np.array(Data_list)
 
    try:
        # 在10以内猜x1，在100以内随机取x2
        x_11 = 5
        x_12 = 54
        x_21 = 2
        x_22 = 74
        Mu_1 = np.array([x_11, x_12])
        Sigma_1 = np.array([[10, 0], [0, 10]])
        Mu_2 = np.array([x_21, x_22])
        Sigma_2 = np.array([[10, 0], [0, 10]])
        Pi_weight = 0.5
        iter_times = EM_iterate_times(Data, Mu_1, Sigma_1, Mu_2, Sigma_2, Pi_weight)
        print(iter_times)
    except Exception as e:
        print(e)
 
 
task_1()
# task_2()
