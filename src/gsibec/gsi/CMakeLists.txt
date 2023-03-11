# BClim
list(APPEND gsi_src_files_list

abstract_ensmod.f90
balmod.F90
berror.F90
bias_predictors.F90
bkerror.F90
bkgcov.F90
bkgvar.f90
bkgvar_rewgt.f90
blendmod.f90
calctends.F90
class_get_pseudo_ensperts.f90
compact_diffs.f90
compute_derived.F90
compute_qvar3d.F90
constants.f90
control2state_ad.F90
control2state.F90
control_vectors.f90
gsi_convert_cv_mod.f90
derivsmod.f90
egrid2agrid_mod.f90
en_perts_io.f90
ensctl2state.F90
ensctl2state_ad.F90
general_commvars_mod.f90
general_specmod.f90
general_spectral_transforms.f90
general_sub2grid_mod.f90
general_transform.f90
genqsat.F90
gengrid_vars.f90
get_derivatives.F90
get_derivatives2.F90
get_gefs_ensperts_dualres.F90
getprs.f90
getuv.F90
getvvel.f90
gridmod.F90
gsi_4dvar.f90
gsi_bundlemod.F90
gsi_enscouplermod.f90
gsi_chemguess_mod.F90
gsi_io.f90
gsi_metguess_mod.F90
gsimod.F90
guess_grids.f90
hybrid_ensemble_parameters.f90
hybrid_ensemble_isotropic.F90
jfunc.f90
m_kinds.F90
looplimits.f90
m_berror_stats.f90
m_grid2sub1var.f90
m_gsibec.F90
m_nc_berror.f90
m_spline.f90
mpeu_mpif.F90
mpeu_util.F90
m_fitcons.f90
m_plib8mat1.f90
m_plib8mat2.f90
m_mpimod.F90
mpl_allreduce.F90
m_rerank.f90
m_rf.f90
m_set_eta.F90
m_stats.f90
normal_rh_to_q.f90
outgrads1.f90
plib8.f90
polcarf.f90
prewgt.F90
raflib.f90
rfdpar.f90
rsearch.F90
simpin1.f90
simpin1_init.f90
smooth_polcarf.f90
smoothrf.F90
smoothwwrf.f90
smoothzrf.f90
state_vectors.f90
stop1.f90
stub_bacio_mod.F90
stub_ensmod.f90
stub_get_pseudo_ensperts.f90
tendsmod.f90
tpause.f90
tpause_t.F90
tv_to_tsen.f90
write_bkgvars_grid.F90
m_gbc4saber.f90

)

# Stubs for sp lib if package is not found
if( NOT sp_FOUND )
  list( APPEND gsi_src_files_list stub_sp.f90 )
endif()

set( gsi_src_files

${gsi_src_files_list}

PARENT_SCOPE
)

