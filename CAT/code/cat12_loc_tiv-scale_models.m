function cat12_LOC_TIVscale_models(TPMname, batch_model, measure, factor , covariates, interaction);
    %
    % This function was written by Alaina Pearce in the Spring of 2020 to
    % run 2nd level models looking at LOC in the compiled structural data.
    % All structural data has previously been preprocessed in Processed
    % directory.
    
    % 
    %     Copyright (C) 2015 Shana Adise
    % 
    %     This program is free software: you can redistribute it and/or modify
    %     it under the terms of the GNU General Public License as published by
    %     the Free Software Foundation, either version 3 of the License, or
    %     (at your option) any later version.
    % 
    %     This program is distributed in the hope that it will be useful,
    %     but WITHOUT ANY WARRANTY; without even the implied warranty of
    %     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    %     GNU General Public License for more details.
    % 
    %     You should have received a copy of the GNU General Public License
    %     along with this program.  If not, see <https://www.gnu.org/licenses/>.

    
    % TPMname: TPM name
    %
    % batch_model: model you want to run - 2sampleT, ANOVA, Reg -
    % needs to correspond to the name used in the batch template
    %
    % measure: structural outcome - volume, corticalthickness, SD, GI, or
    % complexity
    %
    % factor: if running 2sampleT or ANOVA, this is primary factor (other
    % can be added as covariates or interaction)
    %
    % covariates: a structure array with list of covariates - {'age'
    % 'sex'}; need to have curly brackets when you enter it
    %
    % interaction: for ANOVA and 2sampleT this is variable you want to
    % interact with the factor; for Reg this is the variable you want to
    % interact with first covariate
    
    % ==========================================================================================================
    %                                          Preludes: settings, inputs, etc.
    % ==========================================================================================================
    % suppress warning    
    warning('off','MATLAB:prnRenderer:opengl');
    warning('off', 'MATLAB:hg:AutoSoftwareOpenGL');
    
    %for debugging
    %TPM_name = 'all';
    
    %set initial path structures
    %!need to edit this section (and all paths) if move script or any directories it calls!

    %get working directory path for where this script is saved
    %(individual path info '/Box Sync/b-childfoodlab Shared/MRIstruct/Scripts')
    script_wd = mfilename('fullpath');

%    if ismac()
%        slash = '/';
%    else 
%        slash = '\';
%    end
    
    %for us on the computing cluster:
    slash = '/';

    %get location/character number for '/" in file path
    slashloc_wd=find(script_wd==slash);

    %%addpath for spm - work desktop path only
    addpath([script_wd(1:slashloc_wd(3)) 'SPM' slash 'spm12']);
    
    %%addpath for cluster only
    %addpath(('/storage/home/azp271/SPM/spm12'))
    
    %use all characters in path name upto the second to last slash (individual path info
    %'/Box Sync/b-childfoodlab Shared/RO1_Brain_Mechanisms_IRB_5357/MRIstruct/LOCstructural)
    base_wd = script_wd(1:slashloc_wd(end-1));
    CAT_wd = script_wd(1:slashloc_wd(end-2));
    
    %this will tell matlab to look at all files withing the base_wd/CAT--so any
    %subfolder will be added to search path
    result_main_folder=[base_wd slash 'cat12_LOCmodels'];
    
    cat12_LOC_batch = [base_wd slash 'CATscripts' slash 'cat12_LOC_' char(batch_model) '_TIVScale_batchtemplate.mat'];
    
    % get a nice environment
    format short g;
    clc
    fg = spm_figure('Findwin','Graphics');
    fi = spm_figure('Findwin','Interactive');
    spm_figure('Clear',fg);
    spm_figure('Clear',fi);
    
    disp(' ');
    disp(['=== Welcome to ' mfilename ' ===']);
    disp(' ');

    %initialize spm
    %spm('defaults','fmri'); %only need if don't have spm open manually
    spm_jobman('initcfg');


    % ==========================================================================================================
    %                                          Get going
    % ==========================================================================================================
    
    %get covariates string
    if exist('covariates', 'var')
        covar_string = ['_' char([covariates{:}])];
    else
        covar_string = '';
    end
    
    if strcmp(measure, 'volume')
        measure_string = '';
    else
        measure_string = ['_' measure];
    end
    
    if exist('interaction', 'var')
        int_string = ['_int' char(interaction)];
    else
        int_string = '';
    end
    
    % check if already processed
    if exist([result_main_folder slash char(batch_model) '_TIVScale_TPM'  char(TPMname) int_string char(covar_string) char(measure_string) slash 'LOC_' char(batch_model) int_string char(covar_string) char(measure_string) '_matlabbatch.mat'], 'file')
        % subject complete
        disp(' ');
        disp(['  ... LOC_' char(batch_model) int_string char(covar_string) char(measure_string) '_matlabbatch.mat has already been run for tissue probability map:' char(TPMname)]);
    else     
        % start working on the subject
        disp(' ');
        disp(['  ... Running LOC_' char(batch_model) char(covar_string) char(measure_string) '_matlabbatch.mat for tissue probabiity map: ' char(TPMname)]);

        %make QC directory
        if ~exist([result_main_folder slash char(batch_model) '_TIVScale_TPM'  char(TPMname) int_string char(covar_string) char(measure_string)], 'dir')
            mkdir([result_main_folder slash char(batch_model) '_TIVScale_TPM'  char(TPMname) int_string char(covar_string) char(measure_string)]);
        end
        
        %load covars file
        covars_tab = readtable([base_wd slash 'Data' slash 'LOCstructural_covars.csv'], 'Delimiter', ',');
        
        %add full file paths to table
        if strcmp(measure, 'volume')
            path_string = @(v) [char(CAT_wd) slash 'ProcessedData' slash v '_' char(TPMname) slash ...
                'mri' slash 'smwp1' v '_T1.nii,1'];
            covars_tab.full_path = arrayfun(@(S) path_string(char(S)), covars_tab.parID, 'UniformOutput', false);
        elseif strcmp(measure, 'density')
            path_string = @(v) [char(CAT_wd) slash 'ProcessedData' slash v '_' char(TPMname) slash ...
                'mri' slash 'wm' v '_T1.nii,1'];
            covars_tab.full_path = arrayfun(@(S) path_string(char(S)), covars_tab.parID, 'UniformOutput', false);
        elseif strcmp(measure, 'corticalthickness')
            path_string = @(v) [char(CAT_wd) slash 'ProcessedData' slash v '_' char(TPMname) slash ...
                'surf' slash 's15.mesh.thickness.resampled_32k.' v '_T1.gii'];
            covars_tab.full_path = arrayfun(@(S) path_string(char(S)), covars_tab.parID, 'UniformOutput', false);
        end
        
        % ==========================================================================================================
        %                                          create matlabbatch
        % ==========================================================================================================
        
        %load matlab batch file
        clear matlabbatch;
        load(cat12_LOC_batch);
        
        %results/working directory
        matlabbatch{1,1}.spm.stats.factorial_design.dir = cellstr([result_main_folder slash char(batch_model) '_TIVScale_TPM'  char(TPMname) int_string char(covar_string) char(measure_string)]);
        
        %rating data 
        rating_ex = covars_tab.AverageRating > 3;
        
        %global scaling
        matlabbatch{1,1}.spm.stats.factorial_design.globalc.g_user.global_uval = covars_tab.TIV(~rating_ex);
        matlabbatch{1,1}.spm.stats.factorial_design.globalm.gmsca.gmsca_yes.gmscv = mean(covars_tab.TIV(~rating_ex));
        
        %model specificiation
        if strcmp(char(batch_model), '2sampleT')
            if strcmp(char(factor), 'loc') || strcmp(char(factor), 'LOC')
                %LOC group
                fact1_ind = strcmp(covars_tab.loc1, 'Yes');
                matlabbatch{1,1}.spm.stats.factorial_design.des.t2.scans1 = covars_tab.full_path(fact1_ind & ~rating_ex);

                %No LOC group
                matlabbatch{1,1}.spm.stats.factorial_design.des.t2.scans2 = covars_tab.full_path(~fact1_ind & ~rating_ex);
            
            elseif strcmp(char(factor), 'OBstatus') || strcmp(char(factor), 'Obesity')
                fact1_ind = covars_tab.cBodyMass_status == 1;

                %OB group
                matlabbatch{1,1}.spm.stats.factorial_design.des.t2.scans1 = covars_tab.full_path(fact1_ind & ~rating_ex);

                %notOB group
                matlabbatch{1,1}.spm.stats.factorial_design.des.t2.scans2 = covars_tab.full_path(~fact1_ind & ~rating_ex); 

            elseif strcmp(char(factor), 'Sex') || strcmp(char(factor), 'sex')
                fact1_ind = covars_tab.sex == 1;    
                %boy
                matlabbatch{1,1}.spm.stats.factorial_design.des.t2.scans1 = covars_tab.full_path(fact1_ind & ~rating_ex);

                %girl
                matlabbatch{1,1}.spm.stats.factorial_design.des.t2.scans2 = covars_tab.full_path(~fact1_ind & ~rating_ex); 

            end
         elseif strcmp(char(batch_model), 'ANOVA')
            if strcmp(char(factor), 'LOC') || strcmp(char(factor), 'LOC')
                fact1_ind = strcmp(covars_tab.loc1, 'Yes');
            elseif strcmp(char(factor), 'OBstatus') || strcmp(char(factor), 'Obesity')
                fact1_ind = covars_tab.cBodyMass_status == 1;
            elseif strcmp(char(factor), 'Sex') || strcmp(char(factor), 'sex')
                fact1_ind = covars_tab.sex == 1;
            end
            
            matlabbatch{1,1}.spm.stats.factorial_design.des.fd.fact(1).name = char(factor);

            if exist('interaction', 'var')

                if strcmp(interaction, 'OBstatus') || strcmp(interaction, 'Obesity')
                    OB_ind = covars_tab.cBodyMass_status == 1;

                    %Factor1 - OB group
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(1).scans = covars_tab.full_path(fact1_ind & OB_ind & ~rating_ex);
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(1).levels = [1,1];

                    %Factor1 - not OB group
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(2).scans = covars_tab.full_path(fact1_ind & ~OB_ind & ~rating_ex); 
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(2).levels = [1,2];

                    %Not Factor1 - OB group
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(3).scans = covars_tab.full_path(~fact1_ind & OB_ind & ~rating_ex);
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(3).levels = [2,1];

                    %Not Factor1 - not OB group
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(4).scans = covars_tab.full_path(~fact1_ind & ~OB_ind & ~rating_ex); 
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(4).levels = [2,2];

                    %factor
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.fact(2) = matlabbatch{1,1}.spm.stats.factorial_design.des.fd.fact;
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.fact(2).name = 'OBstatus';

                elseif strcmp(interaction, 'Sex') || strcmp(interaction, 'sex')
                    %Boy
                    sex_ind = covars_tab.sex == 1;

                    %Factor1 - boy
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(1).scans = covars_tab.full_path(fact1_ind & sex_ind & ~rating_ex);
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(1).levels = [1,1];

                    %Factor1 - girl
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(2).scans = covars_tab.full_path(fact1_ind & ~sex_ind & ~rating_ex); 
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(2).levels = [1,2];

                    %Not Factor1 - boy
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(3).scans = covars_tab.full_path(~fact1_ind & sex_ind & ~rating_ex);
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(3).levels = [2,1];

                    %Not Factor1 - girl
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(4).scans = covars_tab.full_path(~fact1_ind & ~sex_ind & ~rating_ex); 
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(4).levels = [2,2];

                    %factor
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.fact(2) = matlabbatch{1,1}.spm.stats.factorial_design.des.fd.fact;
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.fact(2).name = 'Sex';
                
                else
                    %Factor1 group
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(1).scans = covars_tab.full_path(fact1_ind & ~rating_ex);

                    %Not Factor1 group
                    matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(2).scans = covars_tab.full_path(~fact1_ind & ~rating_ex); 
                end
            else
                %Factor1 group
                matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(1).scans = covars_tab.full_path(fact1_ind & ~rating_ex);

                %Not Factor1 group
                matlabbatch{1,1}.spm.stats.factorial_design.des.fd.icell(2).scans = covars_tab.full_path(~fact1_ind & ~rating_ex); 
            end    
        elseif strcmp(char(batch_model), 'Reg')
            matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.scans = covars_tab.full_path(~rating_ex); 
        end
        
        if strcmp(measure, 'corticalthickness')
            matlabbatch{1, 1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
            matlabbatch{1, 1}.spm.stats.factorial_design.masking.tm = rmfield(matlabbatch{1, 1}.spm.stats.factorial_design.masking.tm, 'tma');
        end
        
        if exist('interaction', 'var')
            if strcmp(char(batch_model), 'ANOVA')
                
                %if interaction is a factor and using ANOVA dont add as
                %covariate - added above as factor
                if strcmp(interaction, 'OBstatus') || strcmp(interaction, 'Obesity') || strcmp(interaction, 'Sex') || strcmp(interaction, 'sex')
                    add_covar = 0;
                else
                    add_covar = 1;
                end
            end
        else
            add_covar = 0;
        end
        
        %covariates
        if exist('covariates', 'var')
            if strcmp(batch_model, 'Reg')
                nbatch_cov = length({matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov});
                ncov = length(covariates) + add_covar;
            else
                nbatch_cov = length(matlabbatch{1,1}.spm.stats.factorial_design.cov);
                ncov = length(covariates) + add_covar;
            end
        else
           nbatch_cov = length(matlabbatch{1,1}.spm.stats.factorial_design.cov);
           ncov = add_covar;
        end
        
        if strcmp(batch_model, 'Reg')
            %add one because 1st is intercept
            con_num = 0;
            
            for con=1:length(covariates)
                contrast_mat_pos = zeros(1, ncov+1);
                contrast_mat_neg = zeros(1, ncov+1);
            
                if ~strcmp(char(covariates{con}), 'TIV') && ~strcmp(char(covariates{con}), 'tiv')
                    con_num = con_num + 1;
                    contrast_mat_pos(con+1) =  1;
                    contrast_mat_neg(con+1) = -1; 

                    matlabbatch{1,4}.spm.stats.con.consess{1, con_num}.tcon.name = [char(covariates{con}) '_pos'];
                    matlabbatch{1,4}.spm.stats.con.consess{1, con_num}.tcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, con_num}.tcon.weights = contrast_mat_pos;

                    con_num = con_num + 1;
                    matlabbatch{1,4}.spm.stats.con.consess{1, con_num}.tcon.name = [char(covariates{con}) '_neg'];
                    matlabbatch{1,4}.spm.stats.con.consess{1, con_num}.tcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, con_num}.tcon.weights = contrast_mat_neg;
                    
                    if exist('interaction', 'var')
                        contrast_mat_int = [zeros(1, ncov+1);zeros(1, ncov+1)];
                        contrast_mat_int(1, con+1) = 1;
                        %contrast_mat_int(2, con+1) = -1;
                        contrast_mat_int(1, length(covariates)+2) = -1;
                        %contrast_mat_int(2, length(covariates)+2) = 1;
                        
                        con_num = con_num + 1;
                        matlabbatch{1,4}.spm.stats.con.consess{1, con_num}.fcon.name = [char(covariates{con}) int_string];
                        matlabbatch{1,4}.spm.stats.con.consess{1, con_num}.fcon.sessrep = char('none');
                        matlabbatch{1,4}.spm.stats.con.consess{1, con_num}.fcon.weights = contrast_mat_int;
                    
                    end
                end
            end
            
            if ncov < nbatch_cov 
                if ncov == 0
                   fprintf('No covariates entered for Regression.');
                else
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov = matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(1:ncov);
                end
            elseif ncov > nbatch_cov
                for extra = 1:(ncov - nbatch_cov)
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(nbatch_cov + extra) = matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(1);
                end
            end
            
        elseif ncov < nbatch_cov 
            if ncov == 0
               matlabbatch{1,1}.spm.stats.factorial_design.cov = struct([]);
            else
                matlabbatch{1,1}.spm.stats.factorial_design.cov = matlabbatch{1,1}.spm.stats.factorial_design.cov(1:ncov);
            end
        elseif ncov > nbatch_cov
            for extra = 1:(ncov - nbatch_cov)
                matlabbatch{1,1}.spm.stats.factorial_design.cov(nbatch_cov + extra) = matlabbatch{1,1}.spm.stats.factorial_design.cov(1);
            end
        end
        
        if exist('interaction', 'var')
            if strcmp(batch_model, 'ANOVA')
                if strcmp(interaction, 'OBstatus') || strcmp(interaction, 'Obesity') || strcmp(interaction, 'Sex') || strcmp(interaction, 'sex')
                    matlabbatch{1,4}.spm.stats.con.consess{1, 1}.tcon.name = ['posME' int_string];
                    matlabbatch{1,4}.spm.stats.con.consess{1, 1}.tcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, 1}.tcon.weights = [1,-1,1,-1];
                    
                    matlabbatch{1,4}.spm.stats.con.consess{1, 2}.tcon.name = ['negME' int_string];
                    matlabbatch{1,4}.spm.stats.con.consess{1, 2}.tcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, 2}.tcon.weights = [-1,1,-1,1];
                    
                    matlabbatch{1,4}.spm.stats.con.consess{1, 3}.tcon.name = ['posME_LOC'];
                    matlabbatch{1,4}.spm.stats.con.consess{1, 3}.tcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, 3}.tcon.weights = [1,1,-1,-1];
                    
                    matlabbatch{1,4}.spm.stats.con.consess{1, 4}.tcon.name = ['negME_LOC'];
                    matlabbatch{1,4}.spm.stats.con.consess{1, 4}.tcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, 4}.tcon.weights = [-1,-1,1,1];
                    
                    matlabbatch{1,4}.spm.stats.con.consess{1, 5}.fcon.name = ['LOC' int_string];
                    matlabbatch{1,4}.spm.stats.con.consess{1, 5}.fcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, 5}.fcon.weights = [1,-1,-1,1];
                    
                    matlabbatch{1,4}.spm.stats.con.consess{1, 6}.fcon.name = ['LOC' int_string];
                    matlabbatch{1,4}.spm.stats.con.consess{1, 6}.fcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, 6}.fcon.weights = [-1,1,1,-1; 1,-1,-1,1];
                else
                    newcon_num = length(matlabbatch{1,4}.spm.stats.con.consess);
                    
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+1}.fcon.name = ['Interaction' int_string];
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+1}.fcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+1}.fcon.weights = [0,0,1,-1];
                    
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+2}.fcon.name = ['ME' int_string];
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+2}.fcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+2}.fcon.weights = [0,0,1,1; 0,0,-1,-1];
                    
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+3}.tcon.name = ['posME' int_string];
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+3}.tcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+3}.tcon.weights = [0,0,1,1];
                    
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+4}.tcon.name = ['negME' int_string];
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+4}.tcon.sessrep = char('none');
                    matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+4}.tcon.weights = [0,0,-1,-1];
                end
            elseif ~strcmp(batch_model, 'Reg')
                newcon_num = length(matlabbatch{1,4}.spm.stats.con.consess) + 1;
                
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+1}.fcon.name = ['Interaction' int_string];
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+1}.fcon.sessrep = char('none');
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+1}.fcon.weights = [0,0,1,-1];
                
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+2}.fcon.name = ['posME' int_string];
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+2}.fcon.sessrep = char('none');
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+2}.fcon.weights = [0,0,1,1; 0, 0, -1, -1];
                
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+2}.tcon.name = ['posME' int_string];
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+2}.tcon.sessrep = char('none');
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+2}.tcon.weights = [0,0,1,1];

                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+3}.tcon.name = ['negME' int_string];
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+3}.tcon.sessrep = char('none');
                matlabbatch{1,4}.spm.stats.con.consess{1, newcon_num+3}.tcon.weights = [0,0,-1,-1];
            end
        end
       
        for c=1:ncov
            
            if exist('interaction', 'var')
                if strcmp(batch_model, 'ANOVA')
                    if strcmp(interaction, 'OBstatus') || strcmp(interaction, 'Obesity') || strcmp(interaction, 'Sex') || strcmp(interaction, 'sex')
                    	cov_str = char(cellstr(covariates(c)));
                        matlabbatch{1,1}.spm.stats.factorial_design.cov(c).iCFI = 1;
                    else
                        if c == 1
                            cov_str = char(interaction);
                            matlabbatch{1,1}.spm.stats.factorial_design.cov(c).iCFI = 2;
                        else
                            cov_str = char(cellstr(covariates(c - 1)));
                            matlabbatch{1,1}.spm.stats.factorial_design.cov(c).iCFI = 1;
                        end
                    end
                elseif ~strcmp(batch_model, 'Reg')
                    if c == 1
                        cov_str = char(interaction);
                        matlabbatch{1,1}.spm.stats.factorial_design.cov(c).iCFI = 2;
                    else
                        cov_str = char(cellstr(covariates(c - 1)));
                        matlabbatch{1,1}.spm.stats.factorial_design.cov(c).iCFI = 1;
                    end
                else
                    if c == ncov
                        cov_str = char(interaction);
                    else
                        cov_str = char(cellstr(covariates(c)));
                    end
                end
            else
                cov_str = char(cellstr(covariates(c)));
                if ~strcmp(batch_model, 'Reg')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).iCFI = 1;
                end
            end
            
            if ~strcmp(batch_model, 'Reg')
                matlabbatch{1,1}.spm.stats.factorial_design.cov(c).cname = cov_str;
                if strcmp(cov_str, 'age') || strcmp(cov_str, 'Age')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.cAge_yr(fact1_ind & ~rating_ex); covars_tab.cAge_yr(~fact1_ind & ~rating_ex)];  
                elseif strcmp(cov_str, 'sex') || strcmp(cov_str, 'Sex')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.sex(fact1_ind & ~rating_ex); covars_tab.sex(~fact1_ind & ~rating_ex)];  
                elseif strcmp(cov_str, 'BMIperc') || strcmp(cov_str, 'BMIp')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.cBodyMass_p(fact1_ind & ~rating_ex); covars_tab.cBodyMass_p(~fact1_ind & ~rating_ex)];  
                elseif strcmp(cov_str, 'BMI')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.cBodyMass_index(fact1_ind & ~rating_ex); covars_tab.cBodyMass_index(~fact1_ind & ~rating_ex)];  
                elseif strcmp(cov_str, 'AverageRating') || strcmp(cov_str, 'Rating')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.AverageRating(fact1_ind & ~rating_ex); covars_tab.AverageRating(~fact1_ind & ~rating_ex)];  
                elseif strcmp(cov_str, 'IQR') || strcmp(cov_str, 'iqr')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.IQR(fact1_ind & ~rating_ex); covars_tab.IQR(~fact1_ind & ~rating_ex)];  
                elseif strcmp(cov_str, 'sex') || strcmp(cov_str, 'Sex')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.sex(fact1_ind & ~rating_ex); covars_tab.sex(~fact1_ind & ~rating_ex)];  
                elseif strcmp(cov_str, 'study') || strcmp(cov_str, 'Study')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.Study(fact1_ind & ~rating_ex); covars_tab.Study(~fact1_ind & ~rating_ex)];  
                elseif strcmp(cov_str, 'OBstatus') || strcmp(cov_str, 'Obesity') || strcmp(cov_str, 'obesity')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.cBodyMass_status(fact1_ind &~rating_ex); covars_tab.cBodyMass_status(~fact1_ind & ~rating_ex)];
                elseif strcmp(cov_str, 'p85th') || strcmp(cov_str, 'cdc_p85th')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.cdc_p85th(fact1_ind &~rating_ex); covars_tab.cdc_p85th(~fact1_ind & ~rating_ex)];
                elseif strcmp(cov_str, 'p95th') || strcmp(cov_str, 'cdc_p95th')
                    matlabbatch{1,1}.spm.stats.factorial_design.cov(c).c = [covars_tab.cdc_p95th(fact1_ind &~rating_ex); covars_tab.cdc_p95th(~fact1_ind & ~rating_ex)];
                end
            else
                matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).cname = cov_str;
                
                if strcmp(cov_str, 'age') || strcmp(cov_str, 'Age')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.cAge_yr(~rating_ex);  
                elseif strcmp(cov_str, 'sex') || strcmp(cov_str, 'Sex')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.sex(~rating_ex);  
                elseif strcmp(cov_str, 'BMIperc') || strcmp(cov_str, 'BMIp')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.cBodyMass_p(~rating_ex);  
                elseif strcmp(cov_str, 'BMI')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.cBodyMass_index(~rating_ex);  
                elseif strcmp(cov_str, 'AverageRating') || strcmp(cov_str, 'Rating')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.AverageRating(~rating_ex);  
                elseif strcmp(cov_str, 'IQR') || strcmp(cov_str, 'iqr')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.IQR(~rating_ex);  
                elseif strcmp(cov_str, 'sex') || strcmp(cov_str, 'Sex')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.sex(~rating_ex);  
                elseif strcmp(cov_str, 'study') || strcmp(cov_str, 'Study')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.Study(~rating_ex);  
                elseif strcmp(cov_str, 'LOC') || strcmp(cov_str, 'loc')
                    covars_tab.loc1_dummy = strcmp(covars_tab.loc1, 'Yes');
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = double(covars_tab.loc1_dummy(~rating_ex));  
                elseif strcmp(cov_str, 'OBstatus') || strcmp(cov_str, 'Obesity') || strcmp(cov_str, 'obesity')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.cBodyMass_status(~rating_ex); 
                elseif strcmp(cov_str, 'p85th') || strcmp(cov_str, 'cdc_p85th')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.cdc_p85th(~rating_ex);
                elseif strcmp(cov_str, 'p95th') || strcmp(cov_str, 'cdc_p95th')
                    matlabbatch{1,1}.spm.stats.factorial_design.des.mreg.mcov(c).c = covars_tab.cdc_p95th(~rating_ex);
                end
            end       
        end
        
        % ==========================================================================================================
        %                                          Run matlabbatch
        % ==========================================================================================================
       
        %save batch .mat file 
        
        save([result_main_folder slash char(batch_model) '_TIVScale_TPM'  char(TPMname) int_string char(covar_string) char(measure_string) slash 'LOC_' char(batch_model) int_string char(covar_string) char(measure_string) '_TIVScale_matlabbatch.mat'],'matlabbatch');
        
        %run the batch
        spm_jobman('run', matlabbatch);
        
        % ==========================================================================================================
        %                                          Housekeeping
        % ==========================================================================================================

        % say goodbye
        disp(['   ... thank you for using this script and']);
        disp(['=== Have a nice day ===']);
        disp(' ');
    end

    % reset data format
    format;
    return;
end
