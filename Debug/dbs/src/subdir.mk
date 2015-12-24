################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../dbs/src/fvsSQL.c \
../dbs/src/mkdbsTypeDefs.c 

OBJS += \
./dbs/src/fvsSQL.o \
./dbs/src/mkdbsTypeDefs.o 

C_DEPS += \
./dbs/src/fvsSQL.d \
./dbs/src/mkdbsTypeDefs.d 


# Each subdirectory must supply rules for building sources it contributes
dbs/src/%.o: ../dbs/src/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C Compiler'
	gcc -O2 -g -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


