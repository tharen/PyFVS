################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../fire/fofem/src/ansi_mai.c \
../fire/fofem/src/bur_bov.c \
../fire/fofem/src/bur_brn.c \
../fire/fofem/src/fm_fofem.c \
../fire/fofem/src/fof_bcm.c \
../fire/fofem/src/fof_ci.c \
../fire/fofem/src/fof_cm.c \
../fire/fofem/src/fof_co.c \
../fire/fofem/src/fof_disp.c \
../fire/fofem/src/fof_duf.c \
../fire/fofem/src/fof_hsf.c \
../fire/fofem/src/fof_lem.c \
../fire/fofem/src/fof_mrt.c \
../fire/fofem/src/fof_sd.c \
../fire/fofem/src/fof_se.c \
../fire/fofem/src/fof_sgv.c \
../fire/fofem/src/fof_sh.c \
../fire/fofem/src/fof_sha.c \
../fire/fofem/src/fof_soi.c \
../fire/fofem/src/fof_unix.c \
../fire/fofem/src/fof_util.c 

OBJS += \
./fire/fofem/src/ansi_mai.o \
./fire/fofem/src/bur_bov.o \
./fire/fofem/src/bur_brn.o \
./fire/fofem/src/fm_fofem.o \
./fire/fofem/src/fof_bcm.o \
./fire/fofem/src/fof_ci.o \
./fire/fofem/src/fof_cm.o \
./fire/fofem/src/fof_co.o \
./fire/fofem/src/fof_disp.o \
./fire/fofem/src/fof_duf.o \
./fire/fofem/src/fof_hsf.o \
./fire/fofem/src/fof_lem.o \
./fire/fofem/src/fof_mrt.o \
./fire/fofem/src/fof_sd.o \
./fire/fofem/src/fof_se.o \
./fire/fofem/src/fof_sgv.o \
./fire/fofem/src/fof_sh.o \
./fire/fofem/src/fof_sha.o \
./fire/fofem/src/fof_soi.o \
./fire/fofem/src/fof_unix.o \
./fire/fofem/src/fof_util.o 

C_DEPS += \
./fire/fofem/src/ansi_mai.d \
./fire/fofem/src/bur_bov.d \
./fire/fofem/src/bur_brn.d \
./fire/fofem/src/fm_fofem.d \
./fire/fofem/src/fof_bcm.d \
./fire/fofem/src/fof_ci.d \
./fire/fofem/src/fof_cm.d \
./fire/fofem/src/fof_co.d \
./fire/fofem/src/fof_disp.d \
./fire/fofem/src/fof_duf.d \
./fire/fofem/src/fof_hsf.d \
./fire/fofem/src/fof_lem.d \
./fire/fofem/src/fof_mrt.d \
./fire/fofem/src/fof_sd.d \
./fire/fofem/src/fof_se.d \
./fire/fofem/src/fof_sgv.d \
./fire/fofem/src/fof_sh.d \
./fire/fofem/src/fof_sha.d \
./fire/fofem/src/fof_soi.d \
./fire/fofem/src/fof_unix.d \
./fire/fofem/src/fof_util.d 


# Each subdirectory must supply rules for building sources it contributes
fire/fofem/src/%.o: ../fire/fofem/src/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C Compiler'
	gcc -O2 -g -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


