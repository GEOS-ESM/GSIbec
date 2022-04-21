# (C) Copyright 2022 United States Government as represented by the Administrator of the National
#     Aeronautics and Space Administration
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# BClim
set ( bclim_files
balmod.F90
berror.F90
bias_predictors.F90
bkerror.F90
bkgcov.F90
bkgvar.f90
bkgvar_rewgt.f90
blendmod.f90
compact_diffs.f90
compute_qvar3d.F90
constants.f90
control2state_ad.F90
control2state.F90
control_vectors.f90
derivsmod.f90
egrid2agrid_mod.f90
general_commvars_mod.f90
general_specmod.f90
general_spectral_transforms.f90
general_sub2grid_mod.f90
general_transform.f90
gengrid_vars.f90
get.f90
getprs.F90
getuv.F90
gridmod.F90
gsi_bundlemod.F90
gsi_chemguess_mod.F90
gsi_metguess_mod.F90
gsimod.F90
guess_grids.f90
jfunc.f90
m_kinds.F90
looplimits.f90
m_berror_stats.f90
m_grid2sub1var.f90
m_gsibclim.F90
m_nc_berror.f90
mpeu_mpif.F90
mpeu_util.F90
m_mpimod.F90
mpl_allreduce.F90
m_rerank.f90
m_rf.f90
m_set_eta.F90
m_stats.f90
normal_rh_to_q.f90
plib8.f90
polcarf.f90
prewgt.F90
raflib.f90
rfdpar.f90
simpin1.f90
simpin1_init.f90
smooth_polcarf.f90
smoothrf.F90
smoothwwrf.f90
smoothzrf.f90
state_vectors.f90
stop1.f90
stub_bacio_mod.F90
stub_sp.f90
tv_to_tsen.f90
write_bkgvars_grid.F90
m_gbc4saber.f90
)

PREPEND( _p_bclim_files     "gsibclim"     ${bclim_files} )

set( bclim_src_files
    ${_p_bclim_files}
    PARENT_SCOPE
)

