package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.client.ClientRequestDto;
import cn.opensrcdevelop.auth.biz.dto.client.ClientResponseDto;
import cn.opensrcdevelop.auth.biz.dto.client.CreateOrUpdateSecretClientResponseDto;
import cn.opensrcdevelop.auth.biz.entity.Client;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;

public interface ClientService extends IService<Client> {

    Client findByClientId(String clientId);

    CreateOrUpdateSecretClientResponseDto createClient(ClientRequestDto requestDto);

    PageData<ClientResponseDto> list(String keyword, int page, int size);

    ClientResponseDto details(String id);

    void updateClient(ClientRequestDto requestDto);

    CreateOrUpdateSecretClientResponseDto updateClientSecret(String id);

    void deleteClient(String clientId);
}
