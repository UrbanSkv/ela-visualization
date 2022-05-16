% clear all
% mex cec14_func.cpp -DWINDOWS
Xmin=-100;
Xmax=100;
pop_size=100;
dimension=2;
iter_max=5000;
runs=1;
fhd=str2func('cec15_func');

X = lhsdesign(pop_size *dimension, dimension);
X = X*200-200/2;
all_results = cell(1,28);
all_samples = cell(1,28);

for i=[1:15]
    disp(i)
    results_f=cec15_func(X', i);
    all_samples{i}=X;
    all_results{i}=results_f;
end



all_results_mat_200 = cat(1,all_results{:});
all_results_mat_200 = reshape(all_results_mat_200.',1,[]);
all_samples_mat_200 = cat(1,all_samples{:});

save("all_samples_mat_2015_d2_100_b.mat", "all_samples_mat_200")
save("all_results_mat_2015_d2_100_b.mat", "all_results_mat_200")
