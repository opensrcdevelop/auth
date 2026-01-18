#!/usr/bin/env python3
"""
Auth Server API 测试工具类

提供 OAuth2 token 获取和 API 请求功能

使用方式:
    from api_client import AuthAPIClient

    client = AuthAPIClient(
        host="http://auth.local.opensrcdevelop.cn:6543",
        client_id="52cb8d26-a352-4e5c-99a7-d52b8afff3b1",
        client_secret="RF5NNRUOH7PHRC2L6ENR4E354EWLEUQZRHU6K26WDWFZJ5BZKJ5PDJPTCIO6SUAI",
        username="claude",
        password="123456"
    )

    # 发送请求（路径会自动添加 /api/v1 前缀）
    response = client.get("/user/page", params={"page": 1, "size": 10})
    print(response)

    # 下载文件
    template_path = client.download("/user/excel/template", "/tmp")
    print(f"模板已下载到: {template_path}")
"""

import re
import time
from pathlib import Path
from typing import Any, Optional, Union

import requests


class AuthAPIClient:
    """Auth Server API 客户端"""

    def __init__(
        self,
        host: str = "http://auth.local.opensrcdevelop.cn:6543",
        client_id: str = "52cb8d26-a352-4e5c-99a7-d52b8afff3b1",
        client_secret: str = "RF5NNRUOH7PHRC2L6ENR4E354EWLEUQZRHU6K26WDWFZJ5BZKJ5PDJPTCIO6SUAI",
        username: str = "claude",
        password: str = "123456",
        api_prefix: str = "/api/v1",
    ):
        self.host = host.rstrip("/")
        self.client_id = client_id
        self.client_secret = client_secret
        self.username = username
        self.password = password
        self.api_prefix = api_prefix.rstrip("/")
        self._access_token: Optional[str] = None
        self._refresh_token: Optional[str] = None
        self._token_expires_at: float = 0

    @property
    def token_endpoint(self) -> str:
        """获取 token 端点地址"""
        return f"{self.host}{self.api_prefix}/oauth2/token"

    def get_token(self) -> str:
        """使用 OAuth2 密码模式获取 access token"""
        data = {
            "grant_type": "password",
            "client_id": self.client_id,
            "client_secret": self.client_secret,
            "username": self.username,
            "password": self.password,
            "scope": "openid",
        }

        response = requests.post(self.token_endpoint, data=data, timeout=30)
        response.raise_for_status()

        result = response.json()
        self._access_token = result["access_token"]
        self._refresh_token = result.get("refresh_token")
        self._token_expires_at = time.time() + result.get("expires_in", 3600) - 60

        return self._access_token

    def ensure_token(self) -> str:
        """确保有有效的 token，必要时自动刷新"""
        if not self._access_token or time.time() >= self._token_expires_at:
            self.get_token()
        return self._access_token

    def _get_headers(self) -> dict:
        """获取带认证的请求头"""
        return {
            "Authorization": f"Bearer {self.ensure_token()}",
            "Content-Type": "application/json",
        }

    def _build_url(self, path: str) -> str:
        """构建完整 URL"""
        return f"{self.host}{self.api_prefix}{path}"

    def get(
        self,
        path: str,
        params: Optional[dict] = None,
        timeout: int = 30,
    ) -> Any:
        """发送 GET 请求"""
        url = self._build_url(path)
        headers = self._get_headers()
        response = requests.get(url, headers=headers, params=params, timeout=timeout)
        response.raise_for_status()
        return response.json()

    def post(
        self,
        path: str,
        data: Optional[dict] = None,
        params: Optional[dict] = None,
        timeout: int = 30,
    ) -> Any:
        """发送 POST 请求"""
        url = self._build_url(path)
        headers = self._get_headers()
        response = requests.post(url, headers=headers, json=data, params=params, timeout=timeout)
        response.raise_for_status()
        return response.json()

    def put(
        self,
        path: str,
        data: Optional[dict] = None,
        params: Optional[dict] = None,
        timeout: int = 30,
    ) -> Any:
        """发送 PUT 请求"""
        url = self._build_url(path)
        headers = self._get_headers()
        response = requests.put(url, headers=headers, json=data, params=params, timeout=timeout)
        response.raise_for_status()
        return response.json()

    def delete(
        self,
        path: str,
        params: Optional[dict] = None,
        timeout: int = 30,
    ) -> Any:
        """发送 DELETE 请求"""
        url = self._build_url(path)
        headers = self._get_headers()
        response = requests.delete(url, headers=headers, params=params, timeout=timeout)
        response.raise_for_status()
        return response.json()

    def patch(
        self,
        path: str,
        data: Optional[dict] = None,
        params: Optional[dict] = None,
        timeout: int = 30,
    ) -> Any:
        """发送 PATCH 请求"""
        url = self._build_url(path)
        headers = self._get_headers()
        response = requests.patch(url, headers=headers, json=data, params=params, timeout=timeout)
        response.raise_for_status()
        return response.json()

    def download(
        self,
        path: str,
        save_path: Union[str, Path],
        params: Optional[dict] = None,
        timeout: int = 300,
    ) -> Path:
        """下载文件到指定路径

        Args:
            path: API 路径
            save_path: 本地保存路径（目录或完整文件路径）
            params: 请求参数
            timeout: 超时时间（默认 5 分钟）

        Returns:
            保存的文件路径
        """
        url = self._build_url(path)
        headers = self._get_headers()
        # 下载文件时不需要 Content-Type
        headers.pop("Content-Type", None)

        response = requests.get(url, headers=headers, params=params, timeout=timeout, stream=True)
        response.raise_for_status()

        save_path = Path(save_path)

        # 如果是目录，从 Content-Disposition 获取文件名
        if save_path.is_dir() or str(save_path).endswith("/"):
            save_path.mkdir(parents=True, exist_ok=True)
            filename = self._get_filename_from_response(response)
            save_path = save_path / filename

        save_path.parent.mkdir(parents=True, exist_ok=True)

        with open(save_path, "wb") as f:
            for chunk in response.iter_content(chunk_size=8192):
                f.write(chunk)

        return save_path

    def _get_filename_from_response(self, response: requests.Response) -> str:
        """从响应头中提取文件名"""
        content_disposition = response.headers.get("Content-Disposition", "")
        if "filename=" in content_disposition:
            # 尝试解析 filename*= (UTF-8) 或 filename
            match = re.search(r'filename\*=(?:utf-8[\'\"])?([^\'"\r\n]+)', content_disposition)
            if match:
                return match.group(1)
            match = re.search(r'filename=([^\'"\r\n]+)', content_disposition)
            if match:
                return match.group(1)

        # 默认文件名
        return "download"

    def upload(
        self,
        path: str,
        file_path: Union[str, Path],
        file_field_name: str = "file",
        data: Optional[dict] = None,
        timeout: int = 300,
    ) -> Any:
        """上传文件

        Args:
            path: API 路径
            file_path: 要上传的文件路径
            file_field_name: 文件字段名（默认 "file"）
            data: 附加的表单数据
            timeout: 超时时间（默认 5 分钟）

        Returns:
            服务器响应（JSON）
        """
        url = self._build_url(path)
        headers = self._get_headers()
        # 上传文件时不需要 Content-Type，让 requests 自动设置
        headers.pop("Content-Type", None)

        file_path = Path(file_path)
        if not file_path.exists():
            raise FileNotFoundError(f"文件不存在: {file_path}")

        files = {
            file_field_name: (file_path.name, open(file_path, "rb"), self._get_content_type(file_path))
        }

        response = requests.post(url, headers=headers, files=files, data=data, timeout=timeout)
        response.raise_for_status()
        return response.json()

    def _get_content_type(self, file_path: Path) -> str:
        """根据文件扩展名获取 Content-Type"""
        suffix = file_path.suffix.lower()
        content_types = {
            ".xlsx": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            ".xls": "application/vnd.ms-excel",
            ".csv": "text/csv",
            ".json": "application/json",
            ".xml": "application/xml",
            ".txt": "text/plain",
            ".pdf": "application/pdf",
            ".png": "image/png",
            ".jpg": "image/jpeg",
            ".jpeg": "image/jpeg",
            ".gif": "image/gif",
        }
        return content_types.get(suffix, "application/octet-stream")


# 示例用法
if __name__ == "__main__":
    # 创建客户端实例
    client = AuthAPIClient()

    # 示例：获取用户信息
    print("获取当前用户信息:")
    me = client.get("/user/me")
    print(me)

    # 示例：上传文件
    print("\n上传用户 Excel 文件:")
    result = client.upload("/user/excel/import", "/tmp/user_import.xlsx")
    print(result)

